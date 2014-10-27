% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_driver_port ).

% API functions
-export(
    [ start/2
    , start_link/2
    , stop/1
    , request_event/2
    , request_event_async/2
    ] ).

-include( "lurch_driver_protocol.hrl" ).

-define( DRIVER_DIR, code:lib_dir( lurch, drivers ) ).
-ifndef( TEST ).
-define( DRIVER_TIMEOUT, 5000 ).
-else.
-define( DRIVER_TIMEOUT, 100 ).
-endif. % TEST


%% ===================================================================
%% API functions
%% ===================================================================

-spec start( binary(), [ binary() ] ) -> { ok, pid() }.
start( Driver, Parameters ) ->
    Pid = spawn( fun() -> enter_loop( Driver, Parameters ) end ),
    { ok, Pid }.

-spec start_link( binary(), [ binary() ] ) -> { ok, pid() }.
start_link( Driver, Parameters ) ->
    Pid = spawn_link( fun() -> enter_loop( Driver, Parameters ) end ),
    { ok, Pid }.

-spec stop( pid() ) -> ok | { error, timeout }.
stop( Pid ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { stop, From },
    receive
        { stopped, To } -> ok
    after
        ?DRIVER_TIMEOUT -> { error, timeout }
    end.

-spec request_event( pid(), string() ) -> { ok, term() } | { error, timeout }.
request_event( Pid, Event ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { { get_event,  Event }, From },
    receive
        { { event, Result }, To } -> Result
    after
        ?DRIVER_TIMEOUT -> { error, timeout }
    end.

-spec request_event_async( pid(), string() ) -> { ok, { pid(), reference() } }.
request_event_async( Pid, Event ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { { get_event, Event }, From },
    { ok, To }.


%% ===================================================================
%% Internal functions
%% ===================================================================

from_to( From, To ) ->
    Tag = make_ref(),
    { { From, Tag }, { To, Tag } }.

-spec enter_loop( binary(), [ binary() ] ) -> no_return().
enter_loop( Driver, Parameters ) ->
    { ok, Port } = start_driver( Driver, Parameters ),
    receive_loop( Port ).

-spec receive_loop( port() ) -> ok.
receive_loop( Port ) ->
    receive
        { stop, { Pid, Tag } } ->
            stop_driver( Port ),
            Pid ! { stopped, { self(), Tag } };

        { { get_event, Event }, { Pid, Tag } } ->
            Pid ! { { event, get_event( Port, Event ) }, { self(), Tag } },
            receive_loop( Port )
    end.

-spec start_driver( binary(), [ binary() ] ) -> { ok, port() }.
start_driver( Driver, Parameters ) ->
    % TODO - error logging
    try Port = open_port( { spawn_executable, driver_path( Driver ) },
                          [ { args, Parameters }
                          , { line, 8 }
                          , use_stdio ] ),
        { ok, Port }
    catch
        error:Error -> { error, Error }
    end.


-spec stop_driver( port() ) -> ok.
stop_driver( Port ) ->
    { os_pid, Pid } = erlang:port_info( Port, os_pid ),
    erlang:port_close( Port ),
    case lurch_os:is_os_process_alive( Pid ) of
        true -> lurch_os:kill_os_process(9, Pid),
                ok;
        false -> ok
    end.


-spec get_event( port(), string() ) -> { ok, term() }.
get_event( Port, Event ) ->
    true = erlang:port_command( Port, format_cmd( ?EVENT, [ Event ] ) ),
    get_event_acc( Port, [], [], ?DRIVER_TIMEOUT ).

-spec get_event_acc( port(), list(), list(), integer() ) -> list().
get_event_acc( Port, DataAcc, LineAcc, Timeout ) ->
    Start = now(),
    receive
        { Port, { data, { eol, "OK" } } } ->
            Result = lists:map( fun lists:flatten/1, lists:reverse(DataAcc)),
            { ok, Result };

        { Port, { data, { eol, Data } } } ->
            Line = lists:reverse( [ Data | LineAcc ] ),
            ResDataAcc = [ Line | DataAcc ],
            get_event_acc(
              Port, ResDataAcc, [],
              remaining_timeout( Timeout, Start )
            );

        { Port, { data, { noeol, Data } } } ->
            get_event_acc(
              Port, DataAcc, [ Data | LineAcc ],
              remaining_timeout( Timeout, Start )
            )

    after
        Timeout -> throw( { timeout, "Timed out when waiting for event" } )
    end.

-spec remaining_timeout( integer(), erlang:timestamp() ) -> integer().
remaining_timeout( Timeout, Start ) ->
    Remaining = Timeout - ( timer:now_diff( now(), Start ) div 1000 ),
    case Remaining of
        X when X >= 0 -> X;
        X when X  < 0 -> 0
    end.


-spec driver_path( string() ) -> string().
driver_path( Driver ) ->
    case lurch_os:safe_relative_path( Driver ) of
        undefined -> throw( unsafe_relative_path );
        Path -> filename:join( [ ?DRIVER_DIR, Path ] )
    end.


-spec format_cmd( string(), [ string() ] ) -> string().
format_cmd( Cmd, Data ) ->
    string:join( [ Cmd | Data ] ++ ["OK\n"], "\n" ).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).


% Driver tests - lower level

driver_path_test_() ->
    { "driver path",
        { setup,
            fun mock_lurch_os/0,
            fun mock_lurch_os_stop/1,
            [ ?_assert( is_list( driver_path( safe ) ) )
            , ?_assertThrow( unsafe_relative_path, driver_path( unsafe ) )
            ] } }.


driver_start_stop_test_() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    IsPort = erlang:is_port( Port ),
    stop_driver( Port ),
    StoppedPortInfo = erlang:port_info( Port ),
    [ { "port started", ?_assert( IsPort ) }
    , { "port stopped", ?_assertEqual( undefined, StoppedPortInfo ) }
    ].

driver_stuck_test_() ->
    { ok, Port } = start_test_driver( "stuck.sh" ),
    { os_pid, OsPid } = erlang:port_info( Port, os_pid ),
    stop_driver( Port ),
    IsOsPidAliveCmd = lists:flatten( io_lib:format("kill -0 ~b", [ OsPid ] ) ),
    [ { "process killed", ?_assertCmdStatus( 1, IsOsPidAliveCmd ) }
    ].


driver_get_event_test_() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    Res = get_event( Port, "SomeEvent" ),
    ExpData = [ "EVENT", "SomeEvent" ],
    [ { "get event", ?_assertEqual( { ok, ExpData }, Res ) }
    ].


driver_timeout_test_() ->
    { ok, Port } = start_test_driver( "stuck.sh" ),
    [ { "driver timeout"
      , ?_assertThrow( { timeout, _ }, get_event( Port, "SomeEvent" ) ) }
    , { "stop driver" , ?_assertEqual( ok, stop_driver( Port ) ) }
    ].


driver_timeout2_test_() ->
    { ok, Port } = start_test_driver( "garbage.sh" ),
    [ { "driver timeout"
      , ?_assertThrow( { timeout, _ }, get_event( Port, "SomeEvent" ) ) }
    , { "stop driver" , ?_assertEqual( ok, stop_driver( Port ) ) }
    ].


% server tests - API

server_scenario_test_() ->
    { ok, Pid } = start_test_server( "echo.sh" ),
    ProcAlive = is_process_alive( Pid ),

    Res1 = request_event( Pid, "SomeEvent" ),
    { ok, From } = request_event_async( Pid, "SomeEvent" ),
    Res2 = receive
        { { event, E }, From } -> E
    after
        ?DRIVER_TIMEOUT -> timeout
    end,

    Res3 = stop( Pid ),
    ProcAlive2 = is_process_alive( Pid ),

    ExpEventRes = { ok, [ "EVENT", "SomeEvent" ] },

    [ { "server started", ?_assert( ProcAlive ) }
    , { "receive sync event",
        ?_assertEqual( ExpEventRes, Res1 ) }
    , { "receive async event",
        ?_assertEqual( ExpEventRes, Res2 ) }
    , { "server stopped", ?_assertNot( ProcAlive2 ) }
    ].


% Helper functions
test_driver_path( Name ) ->
    filename:join( [ "test", Name ] ).


start_test_driver( Name ) ->
    start_driver( test_driver_path( Name ), [] ).


start_test_server( Name ) ->
    start( test_driver_path( Name ), [] ).


mock_lurch_os() ->
    meck:new( lurch_os, [] ),
    meck:expect( lurch_os, safe_relative_path,
                fun( Path ) ->
                    case Path of
                        safe -> "safe";
                        unsafe -> undefined
                    end
                end ).


mock_lurch_os_stop( _ ) ->
    meck:unload( lurch_os ).


-endif. % TEST

% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_driver_port ).

% API functions
-export(
    [ start/2
    , start_async/2
    , start_link/2
    , start_link_async/2
    , stop/1
    , stop_async/1
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

-spec start( binary(), [ binary() ] ) -> { ok, pid() } | { error, term() }.
start( Driver, Parameters ) ->
    { _, Tag } = From = from(),
    Pid = spawn( fun() -> enter_loop( Driver, Parameters, From ) end ),
    wait_start( { Pid, Tag } ).

-spec start_link( binary(), [ binary() ] ) -> { ok, pid() } | { error, term() }.
start_link( Driver, Parameters ) ->
    { _, Tag } = From = from(),
    Pid = spawn_link( fun() -> enter_loop( Driver, Parameters, From ) end ),
    wait_start( { Pid, Tag } ).

-spec start_async( binary(), [ binary() ] ) -> { ok, { pid(), reference() } }.
start_async( Driver, Parameters ) ->
    { _, Tag } = From = from(),
    Pid = spawn_link( fun() -> enter_loop( Driver, Parameters, From ) end ),
    { ok, { Pid, Tag } }.

-spec start_link_async( binary(), [ binary() ] ) -> { ok, { pid(), reference() } }.
start_link_async( Driver, Parameters ) ->
    { _, Tag } = From = from(),
    Pid = spawn_link( fun() -> enter_loop( Driver, Parameters, From ) end ),
    { ok, { Pid, Tag } }.

-spec stop( pid() ) -> ok | { error, timeout }.
stop( Pid ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { stop, From },
    wait_stop( To ).

-spec stop_async( pid() ) -> { ok, { pid(), reference() } } | { error, timeout }.
stop_async( Pid ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { stop, From },
    { ok, To }.

-spec request_event( pid(), string() ) -> { ok, term() } | { error, timeout }.
request_event( Pid, Event ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { { get_event,  Event }, From },
    wait_event( To ).

-spec request_event_async( pid(), string() ) -> { ok, { pid(), reference() } }.
request_event_async( Pid, Event ) ->
    { From, To } = from_to( self(), Pid ),
    Pid ! { { get_event, Event }, From },
    { ok, To }.


%% ===================================================================
%% Internal functions
%% ===================================================================

-spec from_to( any(), any() ) ->
    { { any(), reference() }, { any(), reference() } }.
from_to( From, To ) ->
    Tag = make_ref(),
    { { From, Tag }, { To, Tag } }.

-spec from( ) -> { any(), reference() }.
from( ) ->
    { self(), make_ref() }.

-spec wait_start( { pid(), reference() } ) -> { ok, pid() } | { error, term() }.
wait_start( Id ) ->
    wait( Id, started ).

-spec wait_stop( { pid(), reference() } ) -> ok | { error, term() }.
wait_stop( Id ) ->
    case wait( Id, stopped ) of
        { ok, _ } -> ok;
        E -> E
    end.

-spec wait_event( { pid(), reference() } ) -> { event, term() } | { error, term() }.
wait_event( Id ) ->
    wait( Id, event ).

-spec wait( { pid(), reference() }, term() ) ->
    { ok, term() } | { error, term() }.
wait( Id, Type ) ->
    receive
        { { Type, Result }, Id } -> Result;
        Error -> Error
    after
        ?DRIVER_TIMEOUT -> { error, timeout }
    end.

-spec enter_loop( string() | binary(),
                  [ string() | binary() ],
                  { pid(), reference() } ) ->
    { { stopped, pid() }, { pid(), reference() } }.
enter_loop( Driver, Parameters, { Pid, Tag } ) ->
    % FIXME - propagate error
    { ok, Port } = start_driver( Driver, Parameters ),
    Pid ! { { started, { ok, self() } }, { self(), Tag } },
    receive_loop( Port ).

-spec receive_loop( port() ) -> { { stopped, pid() }, { pid(), reference() } }.
receive_loop( Port ) ->
    receive
        { stop, { Pid, Tag } } ->
            % FIXME - error propagation
            stop_driver( Port ),
            Pid ! { { stopped, { ok, self() } }, { self(), Tag } };

        { { get_event, Event }, { Pid, Tag } } ->
            Pid ! { { event, get_event( Port, Event ) }, { self(), Tag } },
            receive_loop( Port )
    end.

-spec start_driver( string() | binary(), [ string() | binary() ] ) ->
    { ok, port() }.
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

-spec get_event_acc( port(), list( list() ), list(), integer() ) -> { ok, list() }.
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
    ExpData = { ok, [ "EVENT", "SomeEvent" ] },
    [ { "get event", ?_assertEqual( ExpData, Res ) }
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
    ResEvent = request_event( Pid, "SomeEvent" ),
    ResStop = stop( Pid ),
    ProcAlive2 = is_process_alive( Pid ),
    ExpEventRes = { ok, [ "EVENT", "SomeEvent" ] },

    [ { "server started", ?_assert( ProcAlive ) }
    , { "receive event",
        ?_assertEqual( ExpEventRes, ResEvent ) }
    , { "server stopped",
        ?_assertEqual( ok, ResStop ) }
    , { "server pid not alive", ?_assertNot( ProcAlive2 ) }
    ].

server_scenario_async_test_() ->
    { ok, From } = start_test_server_async( "echo.sh" ),
    { ok, Pid } = receive
        { { started, Res1 }, From } -> Res1
    after
        ?DRIVER_TIMEOUT -> timeout
    end,
    ProcAlive = is_process_alive( Pid ),

    { ok, From2 } = request_event_async( Pid, "SomeEvent" ),
    ResEvent = receive
        { { event, Res2 }, From2 } -> Res2
    after
        ?DRIVER_TIMEOUT -> timeout
    end,

    { ok, From3 } = stop_async( Pid ),
    ResStop = receive
        { { stopped, Res3 }, From3 } -> Res3
    after
        ?DRIVER_TIMEOUT -> timeout
    end,
    ProcAlive2 = is_process_alive( Pid ),
    ExpEventRes = { ok, [ "EVENT", "SomeEvent" ] },

    [ { "server pid alive", ?_assert( ProcAlive ) }
    , { "receive event", ?_assertEqual(
                            { ok, [ "EVENT", "SomeEvent" ] },
                            ResEvent ) }
    , { "server stopped", ?_assertEqual( { ok, Pid } , ResStop ) }
    , { "server pid not alive", ?_assertNot( ProcAlive2 ) }
    ].

% Helper functions
test_driver_path( Name ) ->
    filename:join( [ "test", Name ] ).


start_test_driver( Name ) ->
    start_driver( test_driver_path( Name ), [] ).


start_test_server( Name ) ->
    start( test_driver_path( Name ), [] ).

start_test_server_async( Name ) ->
    start_async( test_driver_path( Name ), [] ).


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

% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_dev ).

-behaviour( gen_server ).

% API functions
-export(
    [ start/2
    , start_link/2
    , stop/1
    , request_event/2
    ] ).

% gen_server callbacks
-export(
    [ init/1, handle_call/3, handle_cast/2
    , handle_info/2, terminate/2, code_change/3
    ] ).

-include( "lurch_dev_proto.hrl" ).

-define( DRIVER_DIR, code:lib_dir( lurch, drivers ) ).
-ifndef( TEST ).
-define( DRIVER_TIMEOUT, 5000 ).
-define( CALL_TIMEOUT, 5100 ).
-else.
-define( DRIVER_TIMEOUT, 100 ).
-define( CALL_TIMEOUT, 200 ).
-endif. % TEST

-type msg_tag() :: reference().

-export_type( [ msg_tag/0 ] ).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start( binary(), [ binary() ] ) -> { ok, pid(), msg_tag() }.
start( Driver, Parameters ) ->
    { _, Tag } = From = from(),
    { ok, Pid } = gen_server:start(
                  ?MODULE,
                  { Driver, Parameters, From }, [] ),
    { ok, Pid, Tag }.


-spec start_link( binary(), [ binary() ] ) -> { ok, pid(), msg_tag() }.
start_link( Driver, Parameters ) ->
    { _, Tag } = From = from(),
    { ok, Pid } = gen_server:start_link(
                  ?MODULE,
                  { Driver, Parameters, From }, [] ),
    { ok, Pid, Tag }.


-spec stop( pid() ) -> { ok, msg_tag() }.
stop( Pid ) ->
    { _, Tag } = From = from(),
    ok = gen_server:cast( Pid, { stop, From } ),
    { ok, Tag }.


-spec request_event( pid(), string() ) -> { ok, msg_tag() }.
request_event( Pid, Event ) ->
    { _, Tag } = From = from(),
    ok = gen_server:cast( Pid, { get_event, Event, From } ),
    { ok, Tag }.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-record( state, { port :: port() | undefined } ).

init( { Driver, Params, From } ) ->
    ok = gen_server:cast( self(), { start_driver, Driver, Params, From } ),
    { ok, #state{} }.


handle_call( stop, _From, State ) ->
    ok = stop_driver( State#state.port ),
    { stop, normal, ok, #state{} };

handle_call( { get_event, Event }, _From, State ) ->
    { reply, get_event( State#state.port, Event ), State }.


handle_cast( { start_driver, Driver, Params, { Pid, Tag } }, #state{} ) ->
    case start_driver( Driver, Params ) of
        { ok, Port } ->
            Pid ! { start, ok, Tag },
            { noreply, #state{ port = Port } };
        Error ->
            Pid ! { start, Error, Tag },
            { stop, Error , #state{ } }
    end;

handle_cast( { stop, { Pid, Tag } }, State ) ->
    ok = stop_driver( State#state.port ),
    Pid ! { stop, ok, Tag },
    { stop, normal, #state{} };

handle_cast( { get_event, Event, { Pid, Tag } }, State ) ->
    Pid ! { event, get_event( State#state.port, Event ), Tag },
    { noreply, State }.


handle_info( _Info, State ) ->
    { noreply, State }.


terminate( normal, _State ) ->
    ok;

terminate( _Reason, State ) ->
    stop_driver( State#state.port ).


code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.


%% ===================================================================
%% Internal functions
%% ===================================================================

-spec from() -> { pid(), reference() }.
from() ->
    { self(), make_ref() }.


-spec start_driver( string() | binary(), [ string() | binary() ] ) ->
    { ok, port() } | { error, term() }.
start_driver( Driver, Parameters ) ->
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
    case erlang:port_info( Port, os_pid ) of
        { os_pid, Pid } -> do_stop_driver( Port, Pid );
        undefined -> ok
    end.

-spec do_stop_driver( port(), non_neg_integer() ) -> ok.
do_stop_driver( Port, Pid ) ->
    catch erlang:port_close( Port ),
    % TODO - give the driver some time to finish cleanly
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

path_test_() ->
    { "driver path",
        { setup,
            fun mock_lurch_os/0,
            fun mock_lurch_os_stop/1,
            [ ?_assert( is_list( driver_path( safe ) ) )
            , ?_assertThrow( unsafe_relative_path, driver_path( unsafe ) )
            ] } }.


start_stop_test_() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    IsPort = erlang:is_port( Port ),
    stop_driver( Port ),
    StoppedPortInfo = erlang:port_info( Port ),
    [ { "port started", ?_assert( IsPort ) }
    , { "port stopped", ?_assertEqual( undefined, StoppedPortInfo ) }
    ].


start_nonexistent_test_() ->
    ?_assertEqual( { error, enoent }, start_test_driver( "nonexistent.sh" ) ).


stuck_test_() ->
    { ok, Port } = start_test_driver( "stuck.sh" ),
    { os_pid, OsPid } = erlang:port_info( Port, os_pid ),
    ResStop = stop_driver( Port ),
    IsOsPidAliveCmd = lists:flatten( io_lib:format("kill -0 ~b", [ OsPid ] ) ),
    [ { "stop driver", ?_assertEqual( ok, ResStop ) }
    , { "process killed", ?_assertCmdStatus( 1, IsOsPidAliveCmd ) }
    ].


get_event_test_() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    Res = get_event( Port, "SomeEvent" ),
    ResStop = stop_driver( Port ),
    ExpData = { ok, [ ?EVENT, "SomeEvent" ] },
    [ { "get event", ?_assertEqual( ExpData, Res ) }
    , { "stop driver", ?_assertEqual( ok, ResStop ) }
    ].


timeout_test_() ->
    { ok, Port } = start_test_driver( "stuck.sh" ),
    [ { "driver timeout"
      , ?_assertThrow( { timeout, _ }, get_event( Port, "SomeEvent" ) ) }
    , { "stop driver" , ?_assertEqual( ok, stop_driver( Port ) ) }
    ].


timeout2_test_() ->
    { ok, Port } = start_test_driver( "garbage.sh" ),
    [ { "driver timeout when sending garbage"
      , ?_assertThrow( { timeout, _ }, get_event( Port, "SomeEvent" ) ) }
    , { "stop driver" , ?_assertEqual( ok, stop_driver( Port ) ) }
    ].

stop_idempotent_test_() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    Tries = 2,
    [ { lists:flatten( io_lib:format( "stop ~B", [ Try ] ) ),
        ?_assertEqual( ok, stop_driver( Port ) ) }
      || Try <- lists:seq( 1, Tries ) ].


% server tests - API

server_scenario_test_() ->
    { ok, Pid, From } = start_test_server( "echo.sh" ),
    ok = receive
        { start, Res1, From } -> Res1
    after
        ?DRIVER_TIMEOUT -> timeout
    end,
    ProcAlive = is_process_alive( Pid ),

    { ok, From2 } = request_event( Pid, "SomeEvent" ),
    ResEvent = receive
        { event, Res2, From2 } -> Res2
    after
        ?DRIVER_TIMEOUT -> timeout
    end,

    { ok, From3 } = stop( Pid ),
    ResStop = receive
        { stop, Res3, From3 } -> Res3
    after
        ?DRIVER_TIMEOUT -> timeout
    end,
    ProcAlive2 = is_process_alive( Pid ),
    ExpEventRes = { ok, [ ?EVENT, "SomeEvent" ] },

    [ { "server pid alive", ?_assert( ProcAlive ) }
    , { "receive event", ?_assertEqual(
                            { ok, [ ?EVENT, "SomeEvent" ] },
                            ResEvent ) }
    , { "server stopped", ?_assertEqual( ok , ResStop ) }
    , { "server pid not alive", ?_assertNot( ProcAlive2 ) }
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

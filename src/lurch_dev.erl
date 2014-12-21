% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_dev ).

-behaviour( gen_server ).

% API functions
-export(
    [ start/3
    , start/4
    , start_link/3
    , start_link/4
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
-else.
-define( DRIVER_TIMEOUT, 100 ).
-endif. % TEST

-type msg_tag() :: reference().
-type device_id() :: term().

-export_type( [ msg_tag/0 ] ).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start( term(), binary(), [ binary() ] ) -> { ok, device_id() }.
start( Id, Driver, Parameters ) ->
    start( Id, Driver, Parameters, self() ).

-spec start( term(), binary(), [ binary() ], pid() ) -> { ok, pid() }.
start( Id, Driver, Parameters, Caller ) ->
    start_server( fun gen_server:start/3, Id, Driver, Parameters, Caller ).


-spec start_link( term(), binary(), [ binary() ] ) -> { ok, device_id() }.
start_link( Id, Driver, Parameters ) ->
    start_link( Id, Driver, Parameters, self() ).

-spec start_link( term(), binary(), [ binary() ], pid() ) -> { ok, pid() }.
start_link( Id, Driver, Parameters, Caller ) ->
    start_server( fun gen_server:start_link/3, Id, Driver, Parameters, Caller ).


-spec stop( term() ) -> ok.
stop( Id ) ->
    gen_server:cast( lurch_proc:via( Id ), { stop, self() } ).


-spec request_event( term(), string() ) -> { ok, msg_tag() }.
request_event( Id, Event ) ->
    { _, Tag } = From = from(),
    ok = gen_server:cast( lurch_proc:via( Id ), { get_event, Event, From } ),
    { ok, Tag }.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-record( state,
    { port :: port() | undefined
    , id :: term()
    } ).

init( { Id, Driver, Params, From } ) ->
    true = lurch_proc:reg( Id ),
    ok = gen_server:cast( self(), { start_driver, Id, Driver, Params, From } ),
    { ok, #state{} }.


handle_call( stop, _From, State ) ->
    ok = stop_driver( State#state.port ),
    { stop, normal, ok, #state{} };

handle_call( { get_event, Event }, _From, State ) ->
    { reply, get_event( State#state.port, Event ), State }.


handle_cast( { start_driver, Id, Driver, Params, From }, #state{} = State0 ) ->
    State1 = State0#state{ id = Id },
    case start_driver( Driver, Params ) of
        { ok, Port } ->
            From ! { start, ok, State1#state.id },
            { noreply, State1#state{ port = Port } };
        Error ->
            From ! { start, Error, Id },
            { stop, Error , State1 }
    end;

handle_cast( { stop, From }, State ) ->
    ok = stop_driver( State#state.port ),
    From ! { stop, ok, State#state.id },
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


start_server( Fun, Id, Driver, Parameters, Caller ) ->
    Fun( ?MODULE, { Id, Driver, Parameters, Caller }, [] ).

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
    { setup
    , fun() -> application:start( gproc ) end
    , fun( _ ) -> application:stop( gproc ) end
    , fun test_server_scenario/1
    }.

test_server_scenario( _ ) ->
    Id = make_ref(),
    { ok, _Pid } = start_test_server( Id, "echo.sh" ),
    Pid = lurch_proc:where( Id ),
    ok = receive
        { start, Res1, Id } -> Res1
    after
        ?DRIVER_TIMEOUT -> timeout
    end,
    ProcAlive = is_process_alive( Pid ),

    { ok, From2 } = request_event( Id, "SomeEvent" ),
    ResEvent = receive
        { event, Res2, From2 } -> Res2
    after
        ?DRIVER_TIMEOUT -> timeout
    end,

    ok = stop( Id ),
    ResStop = receive
        { stop, Res3, Id } -> Res3
    after
        ?DRIVER_TIMEOUT -> timeout
    end,
    ProcAlive2 = is_process_alive( Pid ),

    [ { "server pid alive", ?_assert( ProcAlive ) }
    , { "receive event", ?_assertEqual(
                            { ok, [ ?EVENT, "SomeEvent" ] },
                            ResEvent ) }
    , { "server stopped", ?_assertEqual( ok , ResStop ) }
    , { "server pid not alive", ?_assertNot( ProcAlive2 ) }
    ].

% Helper functions
test_driver_path( Driver ) ->
    filename:join( [ "test", Driver ] ).


start_test_driver( Driver ) ->
    start_driver( test_driver_path( Driver ), [] ).


start_test_server( Id, Driver ) ->
    start( Id, test_driver_path( Driver ), [] ).


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

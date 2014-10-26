% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_driver_port ).
-include( "lurch_driver_protocol.hrl" ).

% API functions
-export(
    [ start_driver/2
    , stop_driver/1
    , get_event/2
    ] ).

-define( DRIVER_DIR, code:lib_dir( lurch, drivers ) ).
-ifndef( TEST ).
-define( DRIVER_TIMEOUT, 5000 ).
-else.
-define( DRIVER_TIMEOUT, 100 ).
-endif. % TEST


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_driver( binary(), [ binary() ] ) -> { ok, port() } | { error, term() }.
start_driver( Driver, Parameters ) ->
    try Port = open_port( { spawn_executable, driver_path( Driver ) },
                          [ { args, Parameters }
                          , { line, 8 }
                          , use_stdio ] ),
        { ok, Port }
    catch
        error:Error -> {error, Error}
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
            Duration = timer:now_diff( Start, now() ),
            get_event_acc( Port, ResDataAcc, [], Timeout - Duration );

        { Port, { data, { noeol, Data } } } ->
            Duration = timer:now_diff( Start, now() ),
            get_event_acc( Port, DataAcc, [ Data | LineAcc ], Timeout - Duration )

    after
        ?DRIVER_TIMEOUT -> throw( { timeout, "Timed out when waiting for event" } )
    end.




%% ===================================================================
%% Internal functions
%% ===================================================================

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

driver_path_test_() ->
    { "driver path",
        { setup,
            fun mock_lurch_os/0,
            fun mock_lurch_os_stop/1,
            [ ?_assert( is_list( driver_path( safe ) ) )
            , ?_assertThrow( unsafe_relative_path, driver_path( unsafe ) )
            ] } }.

driver_start_stop_test_() ->
    { "start and stop driver"
    , test_start_stop_driver() }.


stuck_driver_test_() ->
    { "kill stuck driver"
    , test_kill_driver() }.


get_event_test_() ->
    { "reply to event poll"
    , test_get_event() }.

timeout_test_() ->
    { "test driver timeout"
    , test_timeout() }.

% Helper functions
start_test_driver( Name ) ->
    start_driver( filename:join( ["test", Name ] ), [ ] ).

mock_lurch_os() ->
    meck:new( lurch_os, [ ] ),
    meck:expect( lurch_os, safe_relative_path,
                fun( Path ) ->
                    case Path of
                        safe -> "safe";
                        unsafe -> undefined
                    end
                end ).

mock_lurch_os_stop( _ ) ->
    meck:unload( lurch_os ).


% Actual tests
test_start_stop_driver() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    IsPort = erlang:is_port( Port ),
    stop_driver( Port ),
    StoppedPortInfo = erlang:port_info( Port ),
    [ { "port started", ?_assert( IsPort ) }
    , { "port stopped", ?_assertEqual( undefined, StoppedPortInfo ) }
    ].


test_kill_driver() ->
    { ok, Port } = start_test_driver( "stuck.sh" ),
    { os_pid, OsPid } = erlang:port_info( Port, os_pid ),
    stop_driver( Port ),
    IsOsPidAliveCmd = lists:flatten( io_lib:format("kill -0 ~b", [ OsPid ] ) ),
    [ { "process killed", ?_assertCmdStatus( 1, IsOsPidAliveCmd ) }
    ].


test_get_event() ->
    { ok, Port } = start_test_driver( "echo.sh" ),
    Res = get_event( Port, "SomeEvent" ),
    ExpData = [ "EVENT", "SomeEvent" ],
    [ { "get event", ?_assertEqual( { ok, ExpData }, Res ) }
    ].

test_timeout() ->
    { ok, Port } = start_test_driver( "stuck.sh" ),
    [ { "driver timeout"
      , ?_assertThrow( { timeout, _ }, get_event( Port, "SomeEvent" ) ) }
    , { "stop driver" , ?_assertEqual( ok, stop_driver( Port ) ) }
    ].

test_timeout2() ->
    { ok, Port } = start_test_driver( "garbage.sh" ),
    [ { "driver timeout"
      , ?_assertThrow( { timeout, _ }, get_event( Port, "SomeEvent" ) ) }
    , { "stop driver" , ?_assertEqual( ok, stop_driver( Port ) ) }
    ].

-endif. % TEST

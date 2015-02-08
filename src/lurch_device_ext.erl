% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_device_ext ).

-behaviour( lurch_device ).

% lurch_device callbacks
-export(
    [ start_driver/2
    , stop_driver/2
    , get_event/2
    , handle_info/2
    ] ).

-include( "lurch_device_proto.hrl" ).

-ifndef( TEST ).
-define( DRIVER_TIMEOUT, 5000 ).
-else.
-define( DRIVER_TIMEOUT, 100 ). % FIXME - still needed
-endif. % TEST


%% ===================================================================
%% lurch_device callbacks
%% ===================================================================

-record( ctx,
    { port
    , os_pid % probably unneeded??
    } ).

-spec start_driver( string() | binary(), [ string() | binary() ] ) ->
    { ok, term(), term() } | { error, term(), term() }.
start_driver( Driver, Parameters ) ->
    try Port = open_port( { spawn_executable, driver_path( Driver ) },
                          [ { args, Parameters }
                          , { line, 8 }
                          , exit_status
                          , use_stdio ] ),
        OsPid = erlang:port_info( Port, os_pid ),
        { ok, [ OsPid ], #ctx{ port = Port, os_pid = OsPid } }
    catch
        error:Error -> { error, Error, #ctx{} }
    end.

-spec stop_driver( term(), term() ) -> ok.
stop_driver( _Reason, Ctx ) ->
    Port = Ctx#ctx.port,
    Result = case erlang:port_info( Port, os_pid ) of
        { os_pid, Pid } -> do_stop_driver( Port, Pid );
        undefined -> ok
    end,
    { Result, stop, #ctx{} }.

handle_info( { Port, { exit_status, ExitCode } }, Ctx )
    when Port =:= Ctx#ctx.port ->
    { error, { exit, ExitCode }, Ctx }.


%% ===================================================================
%% Internal functions
%% ===================================================================

-spec do_stop_driver( port(), non_neg_integer() ) -> ok.
do_stop_driver( Port, Pid ) ->
    catch erlang:port_close( Port ),
    % TODO - give the driver some time to finish cleanly
    case lurch_os:is_os_process_alive( Pid ) of
        true -> lurch_os:kill_os_process_group(9, Pid),
                ok;
        false -> ok
    end.


-spec get_event( term(), term() ) -> { ok, term(), term() }.
get_event( Event, Ctx ) ->
    Port = Ctx#ctx.port,
    true = erlang:port_command( Port, format_cmd( ?EVENT, [ Event ] ) ),
    Result = get_event_acc( Port, [], [], ?DRIVER_TIMEOUT ),
    { ok, Result, Ctx }.

% FIXME - refactor
-spec get_event_acc( port(), list( list() ), list(), integer() ) -> list().
get_event_acc( Port, DataAcc, LineAcc, Timeout ) ->
    Start = now(),
    receive
        { Port, { data, { eol, "OK" } } } ->
            lists:map( fun lists:flatten/1, lists:reverse(DataAcc));

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
        undefined -> throw( { unsafe_relative_path, Driver } );
        Path -> filename:join( [ lurch_util:priv_dir( drivers ) , Path ] )
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
            fun lurch_mock:lurch_os_safe_relative_path/0,
            fun lurch_mock:module_stop/1,
            [ ?_assert( is_list( driver_path( safe ) ) )
            , ?_assertThrow( { unsafe_relative_path, unsafe }, driver_path( unsafe ) )
            ] } }.

% FIXME - move relevant tests to lurch_device

start_stop_test_() ->
    { ok, _Info, Ctx } = start_test_driver( "echo.sh" ),
    IsPort = erlang:is_port( Ctx#ctx.port ),
    stop_driver( shutdown, Ctx ),
    StoppedPortInfo = erlang:port_info( Ctx#ctx.port ),
    [ { "port started", ?_assert( IsPort ) }
    , { "port stopped", ?_assertEqual( undefined, StoppedPortInfo ) }
    ].


start_nonexistent_test_() ->
    ?_assertMatch( { error, enoent, #ctx{} }, start_test_driver( "nonexistent.sh" ) ).


stuck_test_() ->
    { ok, _Info, Ctx } = start_test_driver( "stuck.sh" ),
    { os_pid, OsPid } = erlang:port_info( Ctx#ctx.port, os_pid ),
    ResStop = stop_driver( shutdown, Ctx ),
    IsOsPidAliveCmd = lists:flatten( io_lib:format("kill -0 ~b", [ OsPid ] ) ),
    [ { "stop driver", ?_assertMatch( { ok, _, _ }, ResStop ) }
    , { "process killed", ?_assertCmdStatus( 1, IsOsPidAliveCmd ) }
    ].


get_event_test_() ->
    { ok, _Info, Ctx } = start_test_driver( "echo.sh" ),
    Res = get_event( "SomeEvent", Ctx ),
    ResStop = stop_driver( shutdown, Ctx ),
    [ { "get event", ?_assertMatch( { ok, [ ?EVENT, "SomeEvent" ], _ }, Res ) }
    , { "stop driver", ?_assertMatch( { ok, _, _ }, ResStop ) }
    ].


timeout_test_() ->
    { ok, _Info, Ctx } = start_test_driver( "stuck.sh" ),
    [ { "driver timeout"
      , ?_assertThrow( { timeout, _ }, get_event( "SomeEvent", Ctx ) ) }
    , { "stop driver" , ?_assertMatch( { ok, _, _ }, stop_driver( shutdown, Ctx ) ) }
    ].


timeout2_test_() ->
    { ok, _Info, Ctx } = start_test_driver( "garbage.sh" ),
    [ { "driver timeout when sending garbage"
      , ?_assertThrow( { timeout, _ }, get_event( "SomeEvent", Ctx ) ) }
    , { "stop driver" , ?_assertMatch( { ok, _, _ }, stop_driver( shutdown, Ctx ) ) }
    ].

% FIXME - not sure if relevant
%       - indepotency should be likely handled by lurch_devman
stop_idempotent_test_() ->
    { ok, _Info, Ctx } = start_test_driver( "echo.sh" ),
    Tries = 2,
    [ { lists:flatten( io_lib:format( "stop ~B", [ Try ] ) ),
        ?_assertMatch( { ok, _, _ }, stop_driver( shutdown, Ctx ) ) }
      || Try <- lists:seq( 1, Tries ) ].


% Helper functions
test_driver_path( Driver ) ->
    filename:join( [ "test", Driver ] ).


start_test_driver( Driver ) ->
    start_driver( test_driver_path( Driver ), [] ).

-endif. % TEST

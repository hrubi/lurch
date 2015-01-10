-module( lurch_device_subsystem_tests ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

devman_test_() ->
    { foreach
    , fun start_devman/0
    , fun stop_devman/1
    ,
    [ fun test_started/1
    , fun test_single/1
    , fun test_crash/1
    ] }.

test_started( _ ) ->
    [ ?_assert( undefined =/= whereis( lurch_devman ) ) ].

test_single( _ ) ->
    { ok, Id } = lurch_devman:start_device( driver_config_single() ),
    { ok, DevList1 } = lurch_devman:list_devices(),
    [ DevProps ] = lists:filter(
        fun( Props ) ->
            proplists:get_value( id, Props ) =:= Id
        end,
        DevList1 ),
    ok = lurch_devman:stop_device( Id ),
    timer:sleep( 100 ),
    { ok, DevList2 } = lurch_devman:list_devices(),
    [ ?_assertEqual( 1, length( DevList1 ) )
    , ?_assertEqual( "test/echo.sh", proplists:get_value( driver, DevProps ) )
    , ?_assertEqual( 0, length( DevList2 ) )
    ].

test_crash( _ ) ->
    { ok, _Id } = lurch_devman:start_device( driver_config_crash() ),
    % FIXME - the device start should be synchronous for the calling client
    timer:sleep( 100 ),
    { ok, [ DevProps ] } = lurch_devman:list_devices(),
    St = proplists:get_value( state, DevProps ),
    [ ?_assertEqual( crashed, St ) ].

start_devman() ->
    ok = application:start( gproc ),
    ok = application:start( lurch ).

stop_devman( _ ) ->
    ok = application:stop( gproc ),
    ok = application:stop( lurch ).

% Helper functions
driver_config_single() ->
    [ { driver, "test/echo.sh" }
    , { parameters, [] }
    , { events, [] }
    ].


driver_config_crash() ->
    [ { driver, "test/crash.sh" }
    , { parameters, [] }
    , { events, [ ] }
    ].

-endif. % TEST

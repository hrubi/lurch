-module( lurch_device_tests ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

start_stop_test_() ->
    { foreach
    , fun start_devman/0
    , fun stop_devman/1
    ,
    [ fun test_started/1
    , fun test_single/1
    ] }.

test_started( _ ) ->
    [ ?_assert( undefined =/= whereis( lurch_devman ) ) ].

test_single( _ ) ->
    { ok, Id } = lurch_devman:start_device( driver_config() ),
    { ok, DevList1 } = lurch_devman:list_devices(),
    [ DevProps ] = lists:filter(
        fun( Props ) ->
            proplists:get_value( id, Props ) =:= Id
        end,
        DevList1 ),
    ok = lurch_devman:stop_device( Id ),
    { ok, DevList2 } = lurch_devman:list_devices(),
    [ ?_assertEqual( 1, length( DevList1 ) )
    , ?_assertEqual( "test/single.sh", proplists:get_value( driver, DevProps ) )
    , ?_assertEqual( 0, length( DevList2 ) )
    ].

start_devman() ->
    ok = application:start( gproc ),
    ok = application:start( lurch ).

stop_devman( _ ) ->
    ok = application:stop( gproc ),
    ok = application:stop( lurch ).

% Helper functions
driver_config() ->
    [ { driver, "test/single.sh" }
    , { parameters, [] }
    , { events, [ <<"blah">> ] }
    ].

-endif. % TEST

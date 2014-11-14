-module( lurch_system_test ).

-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

start_stop_test_() ->
    { setup
    , fun start_lurch/0
    , fun stop_lurch/1
    , fun test_started/1
    }.

test_started( _ ) ->
    Apps = application:which_applications(),
    [ ?_assertMatch( { lurch, _, _ }, lists:keyfind( lurch, 1, Apps )  ) ].

start_lurch() ->
    ok = application:start( gproc ),
    ok = application:start( lurch ).

stop_lurch( _ ) ->
    ok = application:stop( gproc ),
    ok = application:stop( lurch ).


-endif. % TEST

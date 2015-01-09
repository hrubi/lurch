% @copyright 2014, Jan Hruban
%
% @doc Those tests have to be in separate module, due to the fact that
% it's needed to mock specific function from the tested module. I think
% that if these tests are in the same module which is mecked, meck can't
% cope with that.


-module( lurch_util_tests ).

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( PRIV_PATH, "/path/to/priv_dir" ).

priv_dir_test_() ->
    { setup
    , fun meck_priv_dir/0
    , fun meck_priv_dir_stop/1
    , [ ?_assertEqual( ?PRIV_PATH ++ "/test", lurch_util:test_priv_dir() )
    , ?_assertEqual( ?PRIV_PATH ++ "/test/a", lurch_util:test_priv_dir( a ) )
    , ?_assertEqual( ?PRIV_PATH ++ "/test/a/b", lurch_util:test_priv_dir( [ a, b ] ) )
    , ?_assertEqual( ?PRIV_PATH ++ "/a", lurch_util:priv_dir( a ) )
    , ?_assertEqual( ?PRIV_PATH ++ "/a/b", lurch_util:priv_dir( [ a, b ] ) )
    ] }.

meck_priv_dir() ->
    meck:new( lurch_util, [ passthrough, no_link ] ),
    meck:expect( lurch_util, priv_dir, fun( ) -> ?PRIV_PATH end ).

meck_priv_dir_stop( _ ) ->
    meck:unload( lurch_util ).

-endif. % TEST

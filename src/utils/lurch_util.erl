% @copyright 2014, Jan Hruban
%
% @doc Utilities

-module( lurch_util ).

-export(
    [ priv_dir/0
    , priv_dir/1
    , test_priv_dir/0
    , test_priv_dir/1
    ] ).

priv_dir() ->
    code:priv_dir( lurch ).

priv_dir( List ) when is_list( List ) ->
    filename:join( [ lurch_util:priv_dir() ] ++ List );
priv_dir( Path ) ->
    filename:join( lurch_util:priv_dir(), Path ).

test_priv_dir() ->
    priv_dir( test ).

test_priv_dir( List ) when is_list( List ) ->
    priv_dir( [ test ] ++ List );
test_priv_dir( Path ) ->
    priv_dir( [ test, Path ] ).

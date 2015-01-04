% @copyright 2007, Mochi Media, Inc.
%            2014, Jan Hruban
%
% @doc OS level utility functions

-module( lurch_os ).

-export(
    [ cmd/1
    , is_os_process_alive/1
    , kill_os_process/2
    , safe_relative_path/1
    ] ).


-spec cmd( string() ) -> { integer(), string() }.
cmd( Command ) ->
    eunit_lib:command( lists:flatten( Command ) ).

-spec kill_os_process( integer(), integer() ) -> ok | { error, { integer(), string() } }.
kill_os_process( Signal, Pid ) ->
    { Ret, Out } = cmd( io_lib:format( "kill -s ~b ~b", [ Signal, Pid ] ) ),
    case Ret of
        0 -> ok;
        Ret -> { error, { Ret, Out } }
    end.

-spec is_os_process_alive( integer() ) -> boolean().
is_os_process_alive( Pid ) ->
    case kill_os_process( 0, Pid ) of
        ok -> true;
        _ -> false
    end.

%% Taken from mochiweb_util.erl
%% @doc Inspired by Python 2.5's str.partition:
%%      partition("foo/bar", "/") = {"foo", "/", "bar"},
%%      partition("foo", "/") = {"foo", "", ""}.
-spec partition( string(), string() ) -> { string, [], [] } | { string(), string(), string() }.
partition(String, Sep) ->
    case partition(String, Sep, []) of
        undefined ->
            {String, "", ""};
        Result ->
            Result
    end.

partition("", _Sep, _Acc) ->
    undefined;
partition(S, Sep, Acc) ->
    case partition2(S, Sep) of
        undefined ->
            [C | Rest] = S,
            partition(Rest, Sep, [C | Acc]);
        Rest ->
            {lists:reverse(Acc), Sep, Rest}
    end.

partition2(Rest, "") ->
    Rest;
partition2([C | R1], [C | R2]) ->
    partition2(R1, R2);
partition2(_S, _Sep) ->
    undefined.

%% Taken from mochiweb_util.erl
%% @doc Return the reduced version of a relative path or returns undefined
%%      it is not safe. safe relative paths can be joined with an absolute path and
%%      will result in a subdirectory of the absolute path.
-spec safe_relative_path( string() ) -> string() | undefined.
safe_relative_path("/" ++ _) ->
    undefined;
safe_relative_path(P) ->
    safe_relative_path(P, []).

safe_relative_path("", Acc) ->
    case Acc of
        [] ->
            "";
        _ ->
            string:join(lists:reverse(Acc), "/")
    end;
safe_relative_path(P, Acc) ->
    case partition(P, "/") of
        {"", "/", _} ->
            %% /foo or foo//bar
            undefined;
        {"..", _, _} when Acc =:= [] ->
            undefined;
        {"..", _, Rest} ->
            safe_relative_path(Rest, tl(Acc));
        {Part, "/", ""} ->
            safe_relative_path("", ["", Part | Acc]);
        {Part, _, Rest} ->
            safe_relative_path(Rest, [Part | Acc])
    end.


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

partition_test_() ->
    { "partition",
        [ ?_assertEqual( { "foo", "", "" }, partition( "foo", "/" ) )
        , ?_assertEqual( { "foo", "/", "bar" }, partition( "foo/bar", "/" ) )
        , ?_assertEqual( { "foo", "/", "" }, partition( "foo/", "/" ) )
        , ?_assertEqual( { "", "/", "bar" }, partition( "/bar", "/" ) )
        , ?_assertEqual( { "f", "oo/ba", "r" }, partition( "foo/bar", "oo/ba" ) )
        ] }.

safe_relative_path_test_() ->
    { "safe relative path",
        [ ?_assertEqual( "foo", safe_relative_path( "foo" ) )
        , ?_assertEqual( "foo/", safe_relative_path( "foo/" ) )
        , ?_assertEqual( "foo", safe_relative_path( "foo/bar/.." ) )
        , ?_assertEqual( "bar", safe_relative_path( "foo/../bar" ) )
        , ?_assertEqual( "bar/", safe_relative_path( "foo/../bar/" ) )
        , ?_assertEqual( "", safe_relative_path( "foo/.." ) )
        , ?_assertEqual( "", safe_relative_path( "foo/../" ) )
        , ?_assertEqual( undefined, safe_relative_path( "/foo" ) )
        , ?_assertEqual( undefined, safe_relative_path( "../foo" ) )
        , ?_assertEqual( undefined, safe_relative_path( "foo/../.." ) )
        , ?_assertEqual( undefined, safe_relative_path( "foo//" ) )
        , ?_assertError( { badmatch, _ }, safe_relative_path( <<"../foo">> ) )
        ] }.

-endif. % TEST

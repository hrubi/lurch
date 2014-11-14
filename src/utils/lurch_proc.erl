% @copyright 2014, Jan Hruban
%
% @doc Utilities to cooperate with process registry.

-module( lurch_proc ).

-export(
    [ via/1
    , id/1
    , where/1
    ] ).

-spec via( term() ) -> term().
via( Id ) ->
    { via, gproc, Id }.

-spec id( term() ) -> term().
id( Name ) ->
    { n, l, Name }.

-spec where( term() ) -> pid() | undefined.
where( Id ) ->
    gproc:where( Id ).

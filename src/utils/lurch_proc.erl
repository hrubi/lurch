% @copyright 2014, Jan Hruban
%
% @doc Utilities to cooperate with process registry.

-module( lurch_proc ).

-export(
    [ reg/1
    , via/1
    , id/1
    , where/1
    ] ).

-spec reg( term() ) -> true.
reg( Id ) ->
    gproc:reg( Id ).

-spec via( term() ) -> term().
via( Id ) ->
    { via, gproc, Id }.

-spec id( term() ) -> term().
id( Name ) ->
    { n, l, Name }.

-spec where( term() ) -> pid() | undefined.
where( Id ) ->
    gproc:where( Id ).

% @copyright 2014, Jan Hruban
%
% @doc OS level utility functions

-module( lurch_os ).

-export(
	[ cmd/1
	, is_process_alive/1
	, kill_process/2
	] ).


-spec cmd( string() ) -> { integer(), string() }.
cmd( Command ) ->
	eunit_lib:command( lists:flatten( Command ) ).

-spec kill_process( integer(), integer() ) -> ok | { error, { integer(), string() } }.
kill_process( Signal, Pid ) ->
	{ Ret, Out } = cmd( io_lib:format( "kill -s ~p ~p", [ Signal, Pid ] ) ),
	case Ret of
		0 -> ok;
		Ret -> { error, { Ret, Out } }
	end.

-spec is_process_alive( integer() ) -> boolean().
is_process_alive( Pid ) ->
	case kill_process( 0, Pid ) of
		ok -> true;
		_ -> false
	end.

% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_driver_port ).
-include( "lurch_driver_protocol.hrl" ).

% API functions
-export(
	[ start_driver/2
	, stop_driver/1
	, get_event/2
	] ).

-define( DRIVER_DIR, code:lib_dir( lurch, drivers ) ).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_driver( binary(), [ binary() ] ) -> { ok, port() } | { error, term() }.
start_driver( Driver, Parameters ) ->
	try Port = open_port( { spawn_executable, driver_path( Driver ) },
						  [ { args, Parameters }
						  , { line, 8 }
						  , use_stdio ] ),
		{ ok, Port }
	catch
		error:Error -> {error, Error}
	end.


-spec stop_driver( port() ) -> ok.
stop_driver( Port ) ->
	{ os_pid, Pid } = erlang:port_info( Port, os_pid ),
	erlang:port_close( Port ),
	case lurch_os:is_process_alive( Pid ) of
		true -> lurch_os:kill_process(9, Pid),
				ok;
		false -> ok
	end.


-spec get_event( port(), string() ) -> { ok, term() }.
get_event( Port, Event ) ->
	{ ok, erlang:port_command( Port, format_cmd( ?EVENT, [ Event ] ) ) }.


%% ===================================================================
%% Internal functions
%% ===================================================================

-spec driver_path( string() ) -> string().
driver_path( Driver ) ->
	case lurch_os:safe_relative_path( Driver ) of
		undefined -> throw( unsafe_relative_path );
		Path -> filename:join( [ ?DRIVER_DIR, Path ] )
	end.


-spec format_cmd( string(), [ string() ] ) -> string().
format_cmd( Cmd, Data ) ->
	string:join( [ Cmd | Data ] ++ ["OK\n"], "\n" ).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

driver_path_test_() ->
	{ "driver path",
		{ setup,
			fun mock_lurch_os/0,
			fun mock_lurch_os_stop/1,
			[ ?_assert( is_list( driver_path( safe ) ) )
			, ?_assertThrow( unsafe_relative_path, driver_path( unsafe ) )
			] } }.

driver_start_stop_test_() ->
	{ "start and stop driver"
	, fun test_start_stop_driver/0 }.


stuck_driver_test_() ->
	{ "kill stuck driver"
	, fun test_kill_driver/0 }.


get_event_test_() ->
	{ "reply to event poll"
	, fun test_get_event/0 }.

% Helper functions
start_test_driver( Name ) ->
	start_driver( filename:join( ["test", Name ] ), [ ] ).

mock_lurch_os() ->
	meck:new( lurch_os, [ ] ),
	meck:expect( lurch_os, safe_relative_path,
				fun( Path ) ->
					case Path of
						safe -> "safe";
						unsafe -> undefined
					end
				end ).

mock_lurch_os_stop( _ ) ->
	meck:unload( lurch_os ).


% Actual tests
test_start_stop_driver() ->
	{ ok, Port } = start_test_driver( "echo.sh" ),
	IsPort = erlang:is_port( Port ),
	stop_driver( Port ),
	StoppedPortInfo = erlang:port_info( Port ),
	[ { "port started", ?_assert( IsPort ) }
	, { "port stopped", ?_assertEqual( undefined, StoppedPortInfo ) }
	].


test_kill_driver() ->
	{ ok, Port } = start_test_driver( "stuck.sh" ),
	Pid = erlang:port_info( Port, os_pid ),
	stop_driver( Port ),
	[ { "process killed", ?_assertCmdStatus( 1, io_lib:format("kill -0 ~p", [ Pid ] ) ) }
	].


test_get_event() ->
	{ ok, Port } = start_test_driver( "echo.sh" ),
	Res = get_event( Port, "SomeEvent" ),
	ExpData = "EVENT\nSomeEvent\nOK\n",
	[ { "get event", ?_assertEqual( { ok, ExpData }, Res ) }
	].


-endif. % TEST

% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_driver_port ).

% API functions
-export(
	[ start_driver/2
	, stop_driver/1
	] ).

-define( DRIVER_DIR, code:lib_dir( lurch, drivers ) ).


%% ===================================================================
%% API functions
%% ===================================================================

start_driver( Driver, Parameters ) ->
	try Port = open_port( { spawn_executable, driver_path( Driver ) },
						  [ { args, Parameters }
						  , { line, 8 }
						  , use_stdio ] ),
		io:format("pid: ~p~n", [erlang:port_info(Port, os_pid)]),
		{ ok, Port }
	catch
		error:Error -> {error, Error}
	end.


stop_driver( Port ) ->
	{ os_pid, Pid } = erlang:port_info( Port, os_pid ),
	erlang:port_close( Port ),
	case lurch_os:is_process_alive( Pid ) of
		true -> lurch_os:kill_process(9, Pid),
				ok;
		false -> ok
	end.


driver_path( Driver ) ->
	case filename:pathtype( Driver ) of
		relative -> ok;
		_ -> throw( { error, bad_driver } )
	end,
	case lists:member( "..", filename:split( Driver ) ) of
		true -> throw( { error, bad_driver } );
		false -> ok
	end,
	filename:join( [ ?DRIVER_DIR, Driver ] ).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

% Test descriptions
driver_path_test_( ) ->
	{ "Driver filename sanity checks",
		[ ?_assertThrow( { error, bad_driver }, driver_path( "/absolute/path" ) )
		, ?_assertThrow( { error, bad_driver }, driver_path( "dots/../in/path" ) )
		, ?_assertThrow( { error, bad_driver }, driver_path( "dots/end/.." ) )
		]
	}.


driver_start_stop_test_( ) ->
	{ "Driver can be started and stopped"
	, fun test_start_stop_driver/0 }.


stuck_driver_test_( ) ->
	{ "Driver is killed when does not end on its own"
	, fun test_kill_driver/0 }.


% Helper functions
start_test_driver( Name ) ->
	start_driver( filename:join( ["test", Name ] ), [ ] ).


% Actual tests
test_start_stop_driver( ) ->
	{ ok, Port } = start_test_driver( "echo.sh" ),
	IsPort = erlang:is_port( Port ),
	stop_driver( Port ),
	StoppedPortInfo = erlang:port_info( Port ),
	[ ?_assert( IsPort )
	, ?_assertEqual( undefined, StoppedPortInfo )
	].


test_kill_driver( ) ->
	{ ok, Port } = start_test_driver( "stuck.sh" ),
	Pid = erlang:port_info( Port, os_pid ),
	stop_driver( Port ),
	[ ?_assertCmdStatus( 1, io_lib:format("kill -0 ~p", [ Pid ] ) ) ].


-endif. % TEST

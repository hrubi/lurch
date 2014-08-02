% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_driver_port ).

% API functions
-export(
    [ start_driver/2
	] ).


%% ===================================================================
%% API functions
%% ===================================================================

start_driver( Driver, Parameters ) ->
	Port = try open_port( { spawn_executable, driver_path( Driver ) },
					  [ { args, Parameters }
					  , { line, 8 }
					  , use_stdio ] )
		   catch
			   error:Error -> {error, Error}
		   end,
	{ ok, Port }.


driver_path( Driver ) ->
	case filename:pathtype( Driver ) of
		relative -> ok;
		_ -> throw( { error, bad_driver } )
	end,
	case lists:member( "..", filename:split( Driver ) ) of
		true -> throw( { error, bad_driver } );
		false -> ok
	end,
	filename:join( [ driver_dir( ), Driver ] ).

driver_dir( ) ->
	code:lib_dir( lurch, drivers ).



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
	{ "Driver can be started and stopped",
	  fun test_start_stop_driver/0 }.

% Helper functions
echo_driver( ) -> filename:join( [ "test", "echo.sh" ] ).

% Actual tests
test_start_stop_driver( ) ->
	{ ok, Port } = start_driver( echo_driver( ), [ ]),
	Tests = [ ?_assert( erlang:is_port( Port ) ) ],
	% FIXME - use stop_driver here
	port_close( Port ),
	Tests.


-endif. % TEST

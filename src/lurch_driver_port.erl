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

start_driver( _Driver, _Parameters ) ->
	Port = undefined,
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

driver_path_test_( ) ->
	{ "Driver filename sanity checks",
		[ ?_assertThrow( { error, bad_driver }, driver_path( "/absolute/path" ) )
		, ?_assertThrow( { error, bad_driver }, driver_path( "dots/../in/path" ) )
		, ?_assertThrow( { error, bad_driver }, driver_path( "dots/end/.." ) )
		]
	}.

-endif. % TEST

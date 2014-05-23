-module( lurch_device_driver ).

% API functions
-export(
    [ start_driver/2
	] ).

% API functions

start_driver( _Driver, _Parameters ) ->
	Port = undefined,
	{ ok, Port }.


driver_path( Driver ) ->
	filename:join( [ code:lib_dir( lurch, drivers ), Driver ] ).

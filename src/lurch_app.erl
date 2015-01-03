% @copyright 2014, Jan Hruban


-module( lurch_app ).

-behaviour( application ).

%% Application callbacks
-export( [ start/2, stop/1 ] ).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start( _StartType, _StartArgs ) ->
    { ok, Sup } = lurch_sup:start_link(),
    ok = start_device_subsystem(),
    { ok, Sup }.


stop( _State ) ->
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

start_device_subsystem() ->
    { ok, _Pid } = lurch_sup:start_devman(),
    start_devices(),
    ok.

start_devices() ->
    { ok, App } = application:get_application(),
    Autostart = application:get_env( App, autostart_devices, true ),
    start_devices( Autostart ).

start_devices( true ) ->
    Devices = lurch_conf:read_devices( "test/devices" ),
    lists:foreach(
        fun( Device ) ->
            lurch_devman:start_device( Device )
        end,
        Devices
     );

start_devices( _ ) ->
    ok.

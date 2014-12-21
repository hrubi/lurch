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
    { ok, DevMan } = lurch_sup:start_devman(),
    { ok, _ } = lurch_sup:start_dev_sup(),
    start_devices( DevMan ),
    ok.

start_devices( DevMan ) ->
    Devices = lurch_conf:read_devices( "test/devices" ),
    lists:foreach(
        fun( Device ) ->
            lurch_devman:start_device( DevMan, Device )
        end,
        Devices
     ).

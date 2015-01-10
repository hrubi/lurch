% @copyright 2014, Jan Hruban
%
% @doc Communicates with processes handling port, which does the heavy lifting
% To guarantee good throughput, this module mediates the client requests and
% puts them together.

-module( lurch_devman ).

-behaviour( gen_server ).

% API functions
-export(
    [ start/1, start_link/1, stop/0
    , start_device/1, stop_device/1, list_devices/0
    , poll_device_event/2
    ] ).

% gen_server callbacks
-export(
    [ init/1, handle_call/3, handle_cast/2
    , handle_info/2, terminate/2, code_change/3
    ] ).

-define( SERVER_NAME, ?MODULE ).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start( fun() ) -> ignore | { error, term()} | { ok, pid() }.
start( StartSupFun ) ->
    gen_server:start( { local, ?SERVER_NAME }, ?MODULE, StartSupFun, [] ).

-spec start_link( fun() ) -> ignore | { error, term()} | { ok, pid() }.
start_link( StartSupFun ) ->
    gen_server:start_link( { local, ?SERVER_NAME }, ?MODULE, StartSupFun, [] ).

-spec stop() -> ok.
stop() ->
    gen_server:call( ?SERVER_NAME, stop ).

-type device_id() :: term().

-spec start_device( list() ) -> { ok, device_id() }.
start_device( Configuration ) ->
    gen_server:call( ?SERVER_NAME, { start_device, Configuration } ).

-spec stop_device( device_id() ) -> ok | { error | no_such_device }.
stop_device( Device ) ->
    gen_server:call( ?SERVER_NAME, { stop_device, Device } ).

-spec list_devices() ->  { ok, [ [ proplists:property() ] ] }.
list_devices() ->
    gen_server:call( ?SERVER_NAME, list_devices ).

-spec poll_device_event( device_id(), binary() ) -> ok.
poll_device_event( Device, Event ) ->
    gen_server:cast( ?SERVER_NAME, { poll_device_event, Device, Event } ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-record( device,
    { id         :: term()
    , os_pid     :: non_neg_integer()
    , driver     :: binary()
    , parameters :: [ binary() ]
    , events     :: [ binary() ]
    , state      :: starting | running | stopping | crashed
    , started_by :: term()
    , stopped_by :: term()
    } ).

-record( state,
    { dev_sup
    , devices = orddict:new() :: orddict:orddict()
    , asyncs = orddict:new() :: orddict:orddict()
    } ).


init( StartSupFun ) ->
    self() ! { start_dev_sup, StartSupFun },
    { ok, #state{} }.


handle_call( { start_device, Configuration }, From, State ) ->
    Driver = proplists:get_value(driver, Configuration),
    Parameters = proplists:get_value(parameters, Configuration),
    Events = proplists:get_value(events, Configuration, []),
    { ok, DeviceId } = lurch_device:start(
        State#state.dev_sup, Driver, Parameters, self()
    ),
    Device = #device{ id = DeviceId
                    , driver = Driver
                    , parameters = Parameters
                    , events = Events
                    , state = starting
                    , started_by = From },
    Devices = orddict:store( DeviceId, Device, State#state.devices ),
    NewState = State#state{ devices = Devices },
    { noreply, NewState };

% FIXME - the state of the device should be checked
% eg. can't be possible to stop it when it's already stopping
handle_call( { stop_device, DeviceId }, From, State ) ->
    case orddict:find( DeviceId, State#state.devices ) of
        { ok, Device } ->
            ok = lurch_device:stop( Device#device.id ),
            NewDevices = orddict:update(
                DeviceId,
                fun( D ) -> D#device{ state = stopping, stopped_by = From } end,
                State#state.devices
            ),
            NewState = State#state{ devices = NewDevices },
            { noreply, NewState };
        error -> {reply, { error, no_such_device }, State }
    end;


handle_call( list_devices, _From, State ) ->
    DeviceList = lists:map(
                   fun( { _Key, Val } ) -> device_to_proplist( Val ) end,
                   orddict:to_list( State#state.devices ) ),
    { reply, { ok, DeviceList }, State };


handle_call( stop, _From, State ) ->
    { stop, shutdown, ok, State }.


handle_cast( { poll_device_event, Device, Event }, State ) ->
    case device_event_exists( Device, Event, State#state.devices ) of
        ok ->
            { ok, Tag } = lurch_device:request_event( Device, Event ),
            Asyncs = orddict:store( Tag, Device, State#state.asyncs ),
            { noreply, State#state{ asyncs = Asyncs } };
        _ ->
            % FIXME - log?
            { noreply, State }
    end;

handle_cast( _Request, State ) ->
    { noreply, State }.

handle_info( { start_dev_sup, StartSupFun }, State ) ->
    { ok, SupPid } = StartSupFun(),
    { noreply, State#state{ dev_sup = SupPid } };

handle_info( { event, Msg, Tag }, State ) ->
    handle_async( Tag, Msg, fun handle_poll/3, State );

handle_info( { start, Msg, Id }, State ) ->
    handle_device_transition( Id, Msg, fun handle_start/3, State );

handle_info( { stop, Msg, Id }, State ) ->
    handle_device_transition( Id, Msg, fun handle_stop/3, State ).


terminate( _Reason, _State ) ->
    ok.


code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.



%% ===================================================================
%% Internal functions
%% ===================================================================

device_event_exists( DeviceId, Event, Devices ) ->
    case orddict:find( DeviceId, Devices ) of
        { ok, Device } ->
            case lists:member( Event, Device#device.events ) of
                true -> ok;
                false -> { error, no_such_event }
            end;
        error ->
            { error, no_such_device }
    end.

device_to_proplist( Device ) ->
    [ { id, Device#device.id }
    , { driver, Device#device.driver }
    , { parameters, Device#device.parameters }
    , { events, Device#device.events }
    , { state, Device#device.state }
    , { os_pid, Device#device.os_pid }
    ].

handle_start( { ok, Info }, DeviceId, State ) ->
    reply_start( DeviceId, State, { ok, DeviceId } ),
    UpdateDeviceFun = fun( D ) ->
        D#device{
          state = running,
          os_pid = proplists:get_value( os_pid, Info )
        }
    end,
    Devices = orddict:update(
        DeviceId,
        UpdateDeviceFun,
        State#state.devices
    ),
    State#state{ devices = Devices };

handle_start( { error, _ } = Error, DeviceId, State ) ->
    % FIXME - log
    reply_start( DeviceId, State, Error ),
    Devices = orddict:update(
        DeviceId,
        fun( D ) -> D#device{ state = crashed } end,
        State#state.devices
    ),
    State#state{ devices = Devices }.

reply_start( DeviceId, State, Msg ) ->
    Device = orddict:fetch( DeviceId, State#state.devices ),
    gen_server:reply( Device#device.started_by, Msg ).

handle_stop( shutdown, DeviceId, State ) ->
    reply_stop( DeviceId, State ),
    Devices = orddict:erase( DeviceId, State#state.devices ),
    State#state{ devices = Devices };

handle_stop( _Error, DeviceId, State ) ->
    reply_stop( DeviceId, State ),
    Devices = orddict:update(
        DeviceId,
        fun( D ) -> D#device{ os_pid = undefined, state = crashed } end,
        State#state.devices
    ),
    State#state{ devices = Devices }.

reply_stop( DeviceId, State ) ->
    Device = orddict:fetch( DeviceId, State#state.devices ),
    reply_stop( Device#device.stopped_by ).

% This can happen when there was device stop requested,
% however the device has exited meanwhile.
reply_stop( undefined ) -> ok;
reply_stop( From ) -> gen_server:reply( From, ok ).


handle_poll( _Msg, _DeviceId, State ) ->
    % FIXME - propagate to subscribers
    State.

handle_async( Tag, Msg, Fun, State ) ->
    case orddict:find( Tag, State#state.asyncs ) of
        { ok, DeviceId } ->
            Asyncs = orddict:erase( Tag, State#state.asyncs ),
            State1 = State#state{ asyncs = Asyncs },
            State2 = Fun( Msg, DeviceId, State1 ),
            { noreply, State2 };
        error ->
            { noreply, State }
    end.

handle_device_transition( Id, Msg, Fun, State ) ->
    case orddict:find( Id, State#state.devices ) of
        { ok, _Device } ->
            { noreply, Fun( Msg, Id, State ) };
        error ->
            { stop, { unknown_device, Id }, State }
    end.



%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( DRIVER_NAME, <<"dummy">> ).
-define( EVENT_NAME, <<"event_one">> ).
-define( EVENTS, [ ?EVENT_NAME ] ).
-define( PARAMETERS, [] ).



% Test descriptions
server_test_() ->
    { foreach
    , fun setup_server/0
    , fun setup_server_stop/1
    , [ fun test_start_stop_device/1
      , fun test_add_list_devices/1
      ]
    }.


functional_test_() ->
    { foreach
    , fun setup_meck/0
    , fun setup_meck_stop/1
    , [ fun test_poll_device_event/1
      , fun test_start_response/1
      , fun test_stop_response/1
      ]
    }.



% Actual tests
test_start_stop_device( _ ) ->
    DeviceCount = 2,
    StartResults = [ start_device( dummy_driver_config() ) ||
                    _N <- lists:seq( 1, DeviceCount ) ],
    StopResults = [ stop_device( element( 2, StartResult ) ) ||
                    StartResult <- StartResults ],
    [ [ { "start device", ?_assertEqual( ok, Res ) } || { Res, _ } <- StartResults ]
    , [ { "stop device", ?_assertMatch( ok, Res ) } || Res <- StopResults ]
    ].


test_add_list_devices( _ ) ->
    DeviceCount = 2,
    StartDeviceOk = fun() ->
        { ok, DeviceId } = start_device( dummy_driver_config() ),
        DeviceId
    end,
    DeviceIds = [ StartDeviceOk() || _N <- lists:seq( 1, DeviceCount ) ],
    { ok, Result } = list_devices(),
    GetDeviceFields = fun( Field, Devices ) ->
        [ proplists:get_value( Field, Device ) || Device <- Devices ]
    end,
    [ { "device count", ?_assertEqual( DeviceCount, length( Result ) ) }
    , [ { "device id", ?_assert( lists:member( DeviceId, DeviceIds ) ) }
        || DeviceId <- GetDeviceFields( id, Result ) ]
    , [ { "driver name", ?_assertEqual( ?DRIVER_NAME, Driver ) }
        || Driver <- GetDeviceFields( driver, Result ) ]
    , [ { "parameters", ?_assertEqual( ?PARAMETERS, Driver ) }
        || Driver <- GetDeviceFields( parameters, Result ) ]
    , [ { "events", ?_assertEqual( ?EVENTS, Driver ) }
        || Driver <- GetDeviceFields( events, Result ) ]
    ].


test_poll_device_event( ok ) ->
    Tag = make_ref(),
    meck:expect( lurch_device, request_event,
                 fun( _, _ ) -> { ok, Tag } end ),
    DeviceId = make_ref(),
    Event = myevent,
    Device = #device{ id = DeviceId, events = [ Event ] },
    S0 = #state{ devices = orddict:store( DeviceId, Device, orddict:new() ) },

    { noreply, S1 } = handle_cast( { poll_device_event, DeviceId, Event }, S0 ),
    { noreply, S2 } = handle_cast( { poll_device_event, invalid, Event }, S0 ),
    { noreply, S3 } = handle_cast( { poll_device_event, DeviceId, invalid }, S0 ),

    [ { "valid device and event",
        ?_assertEqual( { ok, DeviceId },
                       orddict:find( Tag, S1#state.asyncs ) ) }
    , { "invalid device", ?_assertEqual( S0, S2 ) }
    , { "invalid event", ?_assertEqual( S0, S3 ) }
    ].

test_start_response( ok ) ->
    Id = make_ref(),
    Device = #device{ id = Id, started_by = { make_ref(), self() } },
    Devices = orddict:store( Id, Device, orddict:new() ),
    S0 = #state{ devices = Devices },

    StartOk = { ok, [ { os_pid, 1234 } ] },
    { noreply, S1 } = handle_info( { start, StartOk, Id }, S0 ),
    { noreply, S2 } = handle_info( { start, { error, reason }, Id }, S0 ),

    Id2 = make_ref(),
    ResNonEx = handle_info( { start, StartOk, Id2 }, S0 ),

    [ { "device id",
        ?_assertEqual( Id, (orddict:fetch( Id, S1#state.devices ) )#device.id ) }
    , { "start result ok", ?_assertEqual(
            running,
            (orddict:fetch( Id, S1#state.devices ))#device.state ) }
    , { "start result error",
        ?_assertMatch(
            #device{ id = Id, state = crashed },
            orddict:fetch( Id, S2#state.devices ) ) }
    , { "nonexisting id",
        ?_assertMatch( { stop, { unknown_device, Id2 }, S0 }, ResNonEx ) }
    ].


test_stop_response( ok ) ->
    Id = make_ref(),
    Id2 = make_ref(),
    Device = #device{ id = Id, state = running,
                      stopped_by = { make_ref(), self() } },
    Devices = orddict:store( Id, Device, orddict:new() ),
    S0 = #state{ devices = Devices },

    { noreply, S1 } = handle_info( { stop, shutdown, Id }, S0 ),
    ResNonEx = handle_info( { stop, shutdown, Id2 }, S0 ),

    { noreply, S2 } = handle_info( { stop, { exit, 1 }, Id }, S0 ),
    ResNonEx2 = handle_info( { stop, { exit, 1 }, Id2 }, S0 ),

    [ { "stop result ok",
        ?_assertEqual( error, orddict:find( Id, S1#state.devices ) ) }
    , { "nonexisting id",
        ?_assertMatch( { stop, { unknown_device, Id2 }, S0 }, ResNonEx ) }

    , { "stop result crash",
        ?_assertMatch(
            #device{ id = Id, state = crashed },
            orddict:fetch( Id, S2#state.devices ) ) }
    , { "nonexisting id",
        ?_assertMatch( { stop, { unknown_device, Id2 }, S0 }, ResNonEx2 ) }
    ].



% setup functions
setup_server() ->
    { ok, _ } = start( fun() -> { ok, make_ref() } end ),
    ok = setup_meck(),
    meck:expect( lurch_device, start,
        fun( _, _, _, _ ) ->
            Id = make_ref(),
            whereis( lurch_devman ) !
            { start, { ok, [ { os_pid, 123 } ] }, Id },
            { ok, Id }
        end
    ),
    meck:expect( lurch_device, stop,
        fun( Id ) ->
            whereis( lurch_devman ) !
                { stop, shutdown, Id },
            ok
        end
   ).

setup_server_stop( _ ) ->
    setup_meck_stop( ok ),
    stop().


setup_meck() ->
    meck:new( lurch_device, [] ),
    ok.


setup_meck_stop( ok ) ->
    meck:unload( lurch_device ).



% Helper functions
dummy_driver_config() ->
    [ { driver, ?DRIVER_NAME }
    , { parameters, ?PARAMETERS }
    , { events, ?EVENTS }
    ].

-endif. % TEST

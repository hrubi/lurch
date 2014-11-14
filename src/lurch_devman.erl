% @copyright 2014, Jan Hruban
%
% @doc Communicates with processes handling port, which does the heavy lifting
% To guarantee good throughput, this module mediates the client requests and
% puts them together.

-module( lurch_devman ).

-behaviour( gen_server ).

% API functions
-export(
    [ start/0, start_link/0, stop/1
    , start_device/2, stop_device/2, list_devices/1
    , poll_device_event/3
    ] ).

% gen_server callbacks
-export(
    [ init/1, handle_call/3, handle_cast/2
    , handle_info/2, terminate/2, code_change/3
    ] ).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start() -> ignore | { error, term()} | { ok, pid() }.
start() ->
    gen_server:start( ?MODULE, [], [] ).

-spec start_link() -> ignore | { error, term()} | { ok, pid() }.
start_link() ->
    gen_server:start_link( ?MODULE, [], [] ).

-spec stop( pid() ) -> ok.
stop( Server ) ->
    gen_server:call( Server, stop ).

-type device_id() :: term().

-spec start_device( pid(), list() ) -> { ok, device_id() }.
start_device( Server, Configuration ) ->
    gen_server:call( Server, { start_device, Configuration } ).

-spec stop_device( pid(), device_id() ) -> ok | { error | no_such_device }.
stop_device( Server, Device ) ->
    gen_server:call( Server, { stop_device, Device } ).

-spec list_devices( pid() ) ->  { ok, [ [ proplists:property() ] ] }.
list_devices( Server ) ->
    gen_server:call( Server, list_devices ).

-spec poll_device_event( pid() , device_id(), binary() ) -> ok.
poll_device_event( Server, Device, Event ) ->
    gen_server:cast( Server, { poll_device_event, Device, Event } ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-record( device,
    { id         :: term()
    , driver     :: binary()
    , parameters :: [ binary() ]
    , events     :: [ binary() ]
    , state      :: starting | running | stopping
    } ).

-record( state,
    { devices = orddict:new() :: orddict:orddict()
    , asyncs = orddict:new() :: orddict:orddict()
    } ).


init( _Args ) ->
    { ok, #state{} }.


handle_call( { start_device, Configuration }, _From, State ) ->
    Driver = proplists:get_value(driver, Configuration),
    Parameters = proplists:get_value(parameters, Configuration),
    Events = proplists:get_value(events, Configuration, []),
    Name = make_ref(),
    { ok, DeviceId } = lurch_dev:start( Name, Driver, Parameters ),
    Device = #device{ id = DeviceId
                    , driver = Driver
                    , parameters = Parameters
                    , events = Events
                    , state = starting },
    Devices = orddict:store( DeviceId, Device, State#state.devices ),
    NewState = State#state{ devices = Devices },
    { reply, { ok, DeviceId }, NewState };

handle_call( { stop_device, DeviceId }, _From, State ) ->
    case orddict:find( DeviceId, State#state.devices ) of
        { ok, _Device } ->
            { reply, ok = lurch_dev:stop( DeviceId ), State };
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
            { ok, Tag } = lurch_dev:request_event( Device, Event ),
            Asyncs = orddict:store( Tag, Device, State#state.asyncs ),
            { noreply, State#state{ asyncs = Asyncs } };
        _ ->
            % FIXME - log?
            { noreply, State }
    end;

handle_cast( _Request, State ) ->
    { noreply, State }.

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
    ].

handle_start( Msg, DeviceId, State ) ->
    case Msg of
        ok ->
            Devices = orddict:update(
                DeviceId,
                fun( D ) -> D#device{ state = running } end,
                State#state.devices
            ),
            State#state{ devices = Devices };
        { error, _Reason } ->
            % FIXME - log
            Devices = orddict:erase( DeviceId, State#state.devices ),
            State#state{ devices = Devices }
    end.

handle_stop( ok, DeviceId, State ) ->
    Devices = orddict:erase( DeviceId, State#state.devices ),
    State#state{ devices = Devices }.

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
            { noreply, State }
    end.



%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( setup_server( F ),
         { setup, fun setup_server/0, fun setup_server_stop/1, fun F/1 } ).

-define( setup_meck( F ),
         { setup, fun setup_meck/0, fun setup_meck_stop/1, fun F/1 } ).

-define( DRIVER_NAME, <<"dummy">> ).

-define( EVENT_NAME, <<"event_one">> ).

% Test descriptions
server_test_() ->
    { "start and stop server"
    , ?setup_server( test_is_alive ) }.


device_start_stop_test_() ->
    { "start and stop device"
    , ?setup_server( test_start_stop_device ) }.


device_list_test_() ->
    { "add and list devices"
    , ?setup_server( test_add_list_devices ) }.


device_poll_event_test_() ->
    { "poll events"
    , ?setup_meck( test_poll_device_event ) }.


device_start_response_test_() ->
    { "start response"
    , ?setup_meck( test_start_response ) }.


device_stop_response_test_() ->
    { "stop response"
    , ?setup_meck( test_stop_response ) }.


% setup functions
setup_server() ->
    { ok, Pid } = start(),
    ok = setup_meck(),
    meck:expect( lurch_dev, start,
                 fun( _Id, _Driver, _Parameters ) -> { ok, make_ref() } end ),
    meck:expect( lurch_dev, stop,
                 fun( _Port ) -> ok end ),
    Pid.


setup_server_stop( Pid ) ->
    setup_meck_stop( ok ),
    stop( Pid ).


setup_meck() ->
    meck:new( lurch_dev, [] ),
    ok.


setup_meck_stop( ok ) ->
    meck:unload( lurch_dev ).



% Actual tests
test_is_alive( Pid ) ->
    [ { "server alive" , ?_assert( erlang:is_process_alive( Pid ) ) } ].


test_start_stop_device( Pid ) ->
    DeviceCount = 2,
    StartResults = [ start_device( Pid, dummy_driver_config() ) ||
                    _N <- lists:seq( 1, DeviceCount ) ],
    StopResults = [ stop_device( Pid, element( 2, StartResult ) ) ||
                    StartResult <- StartResults ],
    [ [ { "start device", ?_assertEqual( ok, Res ) } || { Res, _ } <- StartResults ]
    , [ { "stop device", ?_assertMatch( ok, Res ) } || Res <- StopResults ]
    ].


test_add_list_devices( Pid ) ->
    DeviceCount = 2,
    StartDeviceOk = fun() ->
        { ok, DeviceId } = start_device( Pid, dummy_driver_config() ),
        DeviceId
    end,
    DeviceIds = [ StartDeviceOk() || _N <- lists:seq( 1, DeviceCount ) ],
    { ok, Result } = list_devices( Pid ),
    GetDeviceFields = fun( Field, Devices ) ->
        [ proplists:get_value( Field, Device ) || Device <- Devices ]
    end,
    [ { "device count", ?_assertEqual( DeviceCount, length( Result ) ) }
    , [ { "device id", ?_assert( lists:member( DeviceId, DeviceIds ) ) }
        || DeviceId <- GetDeviceFields( id, Result ) ]
    , [ { "driver name", ?_assertEqual( ?DRIVER_NAME, Driver ) } || Driver <- GetDeviceFields( driver, Result ) ]
    ].


test_poll_device_event( ok ) ->
    Tag = make_ref(),
    meck:expect( lurch_dev, request_event,
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
    Device = #device{ id = Id },
    Devices = orddict:store( Id, Device, orddict:new() ),
    S0 = #state{ devices = Devices },

    { noreply, S1 } = handle_info( { start, ok, Id }, S0 ),
    { noreply, S2 } = handle_info( { start, { error, reason }, Id }, S0 ),
    { noreply, S3 } = handle_info( { start, ok, make_ref() }, S0 ),

    [ { "device id",
        ?_assertEqual( Id, (orddict:fetch( Id, S1#state.devices ) )#device.id ) }
    , { "start result ok", ?_assertEqual(
            running,
            (orddict:fetch( Id, S1#state.devices ))#device.state ) }
    , { "start result error",
        ?_assertEqual( error, orddict:find( Id, S2#state.devices ) ) }
    , { "nonexisting id",
        ?_assertEqual( S0, S3 ) }
    ].


test_stop_response( ok ) ->
    Id = make_ref(),
    Device = #device{ id = Id, state = running },
    Devices = orddict:store( Id, Device, orddict:new() ),
    S0 = #state{ devices = Devices },

    { noreply, S1 } = handle_info( { stop, ok, Id }, S0 ),
    { noreply, S2 } = handle_info( { stop, ok, make_ref() }, S0 ),

    [ { "stop result ok",
        ?_assertEqual( error, orddict:find( Id, S1#state.devices ) ) }
    , { "nonexisting id",
        ?_assertEqual( S0, S2 ) }
    ].



% Helper functions
dummy_driver_config() ->
    [ { driver, ?DRIVER_NAME }
    , { parameters, [] }
    , { events, [ ?EVENT_NAME ] }
    ].

-endif. % TEST

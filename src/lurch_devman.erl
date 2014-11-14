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

-type device_id() :: pid().

-spec start_device( pid(), list() ) -> { ok, device_id() }.
start_device( Server, Configuration ) ->
    gen_server:call( Server, { start_device, Configuration } ).

-spec stop_device( pid(), device_id() ) -> ok | { error | no_such_device }.
stop_device( Server, Pid ) ->
    gen_server:call( Server, { stop_device, Pid } ).

-spec list_devices( pid() ) ->  { ok, [ [ proplists:property() ] ] }.
list_devices( Server ) ->
    gen_server:call( Server, list_devices ).

-spec poll_device_event( pid() , device_id(), binary() ) -> ok.
poll_device_event( Server, Pid, Event ) ->
    gen_server:cast( Server, { poll_device_event, Pid, Event } ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-record( device,
    { pid        :: pid()
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
    { ok, Pid, Tag } = lurch_dev:start( Driver, Parameters ),
    Device = #device{ pid = Pid
                    , driver = Driver
                    , parameters = Parameters
                    , events = Events
                    , state = starting },
    Devices = orddict:store( Pid, Device, State#state.devices ),
    Asyncs = orddict:store( Tag, Pid, State#state.asyncs ),
    NewState = State#state{ devices = Devices, asyncs = Asyncs },
    { reply, { ok, Device#device.pid }, NewState };

handle_call( { stop_device, Pid }, _From, State ) ->
    case orddict:find( Pid, State#state.devices ) of
        { ok, _Device } ->
            { ok, Tag } = lurch_dev:stop( Pid ),
            Asyncs = orddict:store( Tag, Pid, State#state.asyncs ),
            { reply, ok, State#state{ asyncs = Asyncs } };
        error -> {reply, { error, no_such_device }, State }
    end;


handle_call( list_devices, _From, State ) ->
    DeviceList = lists:map(
                   fun( { _Key, Val } ) -> device_to_proplist( Val ) end,
                   orddict:to_list( State#state.devices ) ),
    { reply, { ok, DeviceList }, State };


handle_call( stop, _From, State ) ->
    { stop, shutdown, ok, State }.


handle_cast( { poll_device_event, Pid, Event }, State ) ->
    case device_event_exists( Pid, Event, State#state.devices ) of
        ok ->
            { ok, Tag } = lurch_dev:request_event( Pid, Event ),
            Asyncs = orddict:store( Tag, Pid, State#state.asyncs ),
            { noreply, State#state{ asyncs = Asyncs } };
        _ ->
            % FIXME - log?
            { noreply, State }
    end;

handle_cast( _Request, State ) ->
    { noreply, State }.

handle_info( { event, Msg, Tag }, State ) ->
    handle_async( Tag, Msg, fun handle_poll/3, State );

handle_info( { start, Msg, Tag }, State ) ->
    handle_async( Tag, Msg, fun handle_start/3, State );

handle_info( { stop, Msg, Tag }, State ) ->
    handle_async( Tag, Msg, fun handle_stop/3, State ).


terminate( _Reason, _State ) ->
    ok.


code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.



%% ===================================================================
%% Internal functions
%% ===================================================================

device_event_exists( Pid, Event, Devices ) ->
    case orddict:find( Pid, Devices ) of
        { ok, Device } ->
            case lists:member( Event, Device#device.events ) of
                true -> ok;
                false -> { error, no_such_event }
            end;
        error ->
            { error, no_such_device }
    end.

device_to_proplist( Device ) ->
    [ { pid, Device#device.pid }
    , { driver, Device#device.driver }
    , { parameters, Device#device.parameters }
    , { events, Device#device.events }
    ].

handle_start( Msg, Pid, State ) ->
    case Msg of
        ok ->
            Devices = orddict:update(
                Pid,
                fun( D ) -> D#device{ state = running } end,
                State#state.devices
            ),
            State#state{ devices = Devices };
        { error, _Reason } ->
            % FIXME - log
            Devices = orddict:erase( Pid, State#state.devices ),
            State#state{ devices = Devices }
    end.

handle_stop( ok, Pid, State ) ->
    Devices = orddict:erase( Pid, State#state.devices ),
    State#state{ devices = Devices }.

handle_poll( _Msg, _Pid, State ) ->
    % FIXME - propagate to subscribers
    State.

handle_async( Tag, Msg, Fun, State ) ->
    case orddict:find( Tag, State#state.asyncs ) of
        { ok, Pid } ->
            Asyncs = orddict:erase( Tag, State#state.asyncs ),
            State1 = State#state{ asyncs = Asyncs },
            State2 = Fun( Msg, Pid, State1 ),
            { noreply, State2 };
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
                 fun( _Driver, _Parameters ) -> { ok, make_ref(), make_ref() } end ),
    meck:expect( lurch_dev, stop,
                 fun( _Port ) -> { ok, make_ref() } end ),
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
    , [ { "device pid", ?_assert( lists:member( DeviceId, DeviceIds ) ) }
        || DeviceId <- GetDeviceFields( pid, Result ) ]
    , [ { "driver name", ?_assertEqual( ?DRIVER_NAME, Driver ) } || Driver <- GetDeviceFields( driver, Result ) ]
    ].


test_poll_device_event( ok ) ->
    Tag = make_ref(),
    meck:expect( lurch_dev, request_event,
                 fun( _, _ ) -> { ok, Tag } end ),
    DeviceId = make_ref(),
    Event = myevent,
    Device = #device{ pid = DeviceId, events = [ Event ] },
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
    DeviceId = make_ref(),
    Tag = make_ref(),
    Device = #device{ pid = DeviceId },
    Devices = orddict:store( DeviceId, Device, orddict:new() ),
    Asyncs = orddict:store( Tag, DeviceId, orddict:new() ),
    S0 = #state{ devices = Devices, asyncs = Asyncs },

    { noreply, S1 } = handle_info( { start, ok, Tag }, S0 ),
    { noreply, S2 } = handle_info( { start, { error, reason }, Tag }, S0 ),
    { noreply, S3 } = handle_info( { start, ok, make_ref() }, S0 ),

    [ { "async purged",
        ?_assertEqual( error, orddict:find( Tag, S1#state.asyncs ) ) }
    , { "start result ok", ?_assertEqual(
            running,
            (orddict:fetch( DeviceId, S1#state.devices ))#device.state ) }
    , { "async purged",
        ?_assertEqual( error, orddict:find( Tag, S2#state.asyncs ) ) }
    , { "start result error",
        ?_assertEqual( error, orddict:find( DeviceId, S2#state.devices ) ) }
    , { "nonexisting tag",
        ?_assertEqual( S0, S3 ) }
    ].


test_stop_response( ok ) ->
    DeviceId = make_ref(),
    Tag = make_ref(),
    Device = #device{ pid = DeviceId, state = running },
    Devices = orddict:store( DeviceId, Device, orddict:new() ),
    Asyncs = orddict:store( Tag, DeviceId, orddict:new() ),
    S0 = #state{ devices = Devices, asyncs = Asyncs },

    { noreply, S1 } = handle_info( { stop, ok, Tag }, S0 ),
    { noreply, S2 } = handle_info( { stop, ok, make_ref() }, S0 ),

    [ { "async purged",
        ?_assertEqual( error, orddict:find( Tag, S1#state.asyncs ) ) }
    , { "stop result ok",
        ?_assertEqual( error, orddict:find( DeviceId, S1#state.devices ) ) }
    , { "nonexisting tag",
        ?_assertEqual( S0, S2 ) }
    ].



% Helper functions
dummy_driver_config() ->
    [ { driver, ?DRIVER_NAME }
    , { parameters, [] }
    , { events, [ ?EVENT_NAME ] }
    ].

-endif. % TEST

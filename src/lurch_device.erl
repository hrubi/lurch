% @copyright 2014, Jan Hruban
%
% @doc Communicates with process handling port, which does the heavy lifting
% To guarantee good throughput, this module mediates the client requests and
% puts them together.

-module( lurch_device ).

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
-spec start( ) -> ignore | { error, term()} | { ok, pid() }.
start( ) ->
    gen_server:start( ?MODULE, [], [] ).

-spec start_link( ) -> ignore | { error, term()} | { ok, pid() }.
start_link( ) ->
    gen_server:start_link( ?MODULE, [], [] ).

-spec stop( pid() ) -> ok.
stop( Server ) ->
    gen_server:call( Server, stop ).

-type device_id() :: reference().

-spec start_device( pid(), list() ) -> { ok, device_id() }.
start_device( Server, Configuration ) ->
    gen_server:call( Server, { start_device, Configuration } ).

-spec stop_device( pid(), device_id() ) -> ok | { error | no_such_device }.
stop_device( Server, Id ) ->
    gen_server:call( Server, { stop_device, Id } ).

-spec list_devices( pid() ) ->  { ok, [ [ proplists:property() ] ] }.
list_devices( Server ) ->
    gen_server:call( Server, list_devices ).

-spec poll_device_event( pid() , device_id(), binary() ) -> ok.
poll_device_event( Server, Id, Event ) ->
    gen_server:cast( Server, { poll_device_event, Id, Event } ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-record( device,
    { id         :: device_id()
    , driver     :: binary()
    , parameters :: [ binary() ]
    , events     :: [ binary() ]
    , pid        :: pid()
    } ).

-record( state,
    { devices = orddict:new() :: orddict:orddict()
    , polls = [] :: list( lurch_driver_port:msg_tag() )
    } ).


init( _Args ) ->
    { ok, #state{} }.


handle_call( { start_device, Configuration }, _From, State ) ->
    Driver = proplists:get_value(driver, Configuration),
    Parameters = proplists:get_value(parameters, Configuration),
    Events = proplists:get_value(events, Configuration, []),
    case lurch_driver_port:start( Driver, Parameters ) of
        { ok, Pid } ->
            DeviceId = make_ref( ),
            Device = #device{ id = DeviceId
                            , driver = Driver
                            , parameters = Parameters
                            , events = Events
                            , pid = Pid },
            Devices = orddict:store( DeviceId, Device, State#state.devices ),
            NewState = State#state{ devices = Devices },
            { reply, { ok, Device#device.id }, NewState };
        { error, _ } = Error ->
            { reply, Error, State }
    end;

handle_call( { stop_device, DeviceId }, _From, State ) ->
    case orddict:find( DeviceId, State#state.devices ) of
        { ok, Device } ->
            lurch_driver_port:stop( Device#device.pid ),
            NewDevices = orddict:erase( DeviceId, State#state.devices ),
            NewState = State#state{ devices = NewDevices },
            { reply, ok, NewState };
        error -> {reply, { error, no_such_device }, State }
    end;


handle_call( list_devices, _From, State ) ->
    DeviceList = lists:map(
                   fun( { _Key, Val } ) -> device_to_proplist( Val ) end,
                   orddict:to_list( State#state.devices ) ),
    { reply, { ok, DeviceList }, State };


handle_call( stop, _From, State ) ->
    { stop, shutdown, ok, State }.


handle_cast( { poll_device_event, DeviceId, Event }, State ) ->
    case maybe_get_device_pid( DeviceId, Event, State#state.devices ) of
        { ok, Pid } ->
            Token = lurch_driver_port:request_event_async( Pid, Event ),
            NewState = State#state{ polls = [ Token | State#state.polls ] },
            { noreply, NewState };
        _ ->
            % FIXME - log?
            { noreply, State }
    end;

handle_cast( _Request, State ) ->
    { noreply, State }.


handle_info( _Info, State ) ->
    { noreply, State }.


terminate( _Reason, _State ) ->
    ok.


code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.



%% ===================================================================
%% Internal functions
%% ===================================================================

maybe_get_device_pid( DeviceId, Event, Devices ) ->
    case orddict:find( DeviceId, Devices ) of
        { ok, Device } ->
            case lists:member( Event, Device#device.events ) of
                true -> { ok, Device#device.pid };
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


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( setup( F ), { setup, fun test_start/0, fun test_stop/1, fun F/1 } ).

-define( DRIVER_NAME, <<"dummy">> ).

-define( EVENT_NAME, <<"event_one">> ).

% Test descriptions
server_test_( ) ->
    { "start and stop server"
    , ?setup( test_is_alive ) }.


device_start_stop_test_( ) ->
    { "start and stop device"
    , ?setup( test_start_stop_device ) }.


device_start_error_test_( ) ->
    { "start device error"
    , ?setup( test_start_device_error ) }.


device_list_test_( ) ->
    { "add and list devices"
    , ?setup( test_add_list_devices ) }.


device_poll_event_test_( ) ->
    { "poll events"
    , test_poll_device_event() }.


% setup functions
test_start( ) ->
    { ok, Pid } = start( ),
    meck:new( lurch_driver_port, [] ),
    meck:expect( lurch_driver_port, start,
                 fun( _Driver, _Parameters ) -> { ok, pid_mock } end ),
    meck:expect( lurch_driver_port, stop,
                 fun( _Port ) -> ok end ),
    Pid.


test_stop( Pid ) ->
    meck:unload( lurch_driver_port ),
    stop( Pid ).


% Actual tests
test_is_alive( Pid ) ->
    [ { "server alive" , ?_assert( erlang:is_process_alive( Pid ) ) } ].


test_start_stop_device( Pid ) ->
    DeviceCount = 2,
    StartResults = [ start_device( Pid, dummy_driver_config( ) ) ||
                    _N <- lists:seq( 1, DeviceCount ) ],
    StopResults = [ stop_device( Pid, element( 2, StartResult ) ) ||
                    StartResult <- StartResults ],
    [ { "start device", ?_assert( lists:all( fun( Res ) -> element( 1, Res ) =:= ok end,
                            StartResults ) ) }
    , { "stop device", ?_assert( lists:all( fun( Res ) ->
                            Res =:= ok end, StopResults ) ) }
    ].


test_start_device_error( Pid ) ->
    meck:expect( lurch_driver_port, start,
                 fun( _Driver, _Parameters ) -> { error, enoent } end ),
    StartResult = start_device( Pid, dummy_driver_config( ) ),
    [ { "error propagated", ?_assertMatch( { error, _Error }, StartResult ) } ].


test_add_list_devices( Pid ) ->
    DeviceCount = 2,
    StartDeviceOk = fun( ) ->
        { ok, DeviceId } = start_device( Pid, dummy_driver_config() ),
        DeviceId
    end,
    DeviceIds = [ StartDeviceOk( ) || _N <- lists:seq( 1, DeviceCount ) ],
    { ok, Result } = list_devices( Pid ),
    GetDeviceFields = fun( Field, Devices ) ->
        [ proplists:get_value( Field, Device ) || Device <- Devices ]
    end,
    [ { "device count", ?_assertEqual( DeviceCount, length( Result ) ) }
    , [ { "device id", ?_assert( lists:member( Id, DeviceIds ) ) }
        || Id <- GetDeviceFields( id, Result ) ]
    , [ { "driver name", ?_assertEqual( ?DRIVER_NAME, Driver ) } || Driver <- GetDeviceFields( driver, Result ) ]
    ].


test_poll_device_event() ->
    meck:new( lurch_driver_port, [] ),
    Token = make_ref(),
    meck:expect( lurch_driver_port, request_event_async,
                 fun( _, _ ) -> Token end ),
    DeviceId = make_ref(),
    Event = myevent,
    Device = #device{ id = DeviceId, events = [ Event ] },
    S0 = #state{ devices = orddict:store( DeviceId, Device, orddict:new() ) },

    { noreply, S1 } = handle_cast( { poll_device_event, DeviceId, Event }, S0 ),
    { noreply, S2 } = handle_cast( { poll_device_event, invalid, Event }, S0 ),
    { noreply, S3 } = handle_cast( { poll_device_event, DeviceId, invalid }, S0 ),

    [ { "valid device and event", ?_assertEqual( [ Token ], S1#state.polls ) }
    , { "invalid device", ?_assertEqual( S0, S2 ) }
    , { "invalid event", ?_assertEqual( S0, S3 ) }
    ].


% Helper functions
dummy_driver_config( ) ->
    [ { driver, ?DRIVER_NAME }
    , { parameters, [] }
    , { events, [ ?EVENT_NAME ] }
    ].

-endif. % TEST

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
	gen_server:start( ?MODULE, [ ], [ ] ).

-spec start_link( ) -> ignore | { error, term()} | { ok, pid() }.
start_link( ) ->
	gen_server:start_link( ?MODULE, [ ], [ ] ).

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

-spec poll_device_event( pid() , device_id(), binary() ) ->
	{ ok, term() } | { error, no_such_device } | { error | no_such_event }.
poll_device_event( Server, Id, Event ) ->
	gen_server:call( Server, { poll_device_event, Id, Event } ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-record( device,
	{ id         :: device_id()
	, driver     :: binary()
	, parameters :: [ binary() ]
	, events	 :: [ binary() ]
	, port		 :: port()
	} ).

-record( state,
	{ devices :: orddict:orddict()
	} ).


init( _Args ) ->
	Devices = orddict:new(),
	State = #state{ devices = Devices },
	{ ok, State }.


handle_call( { start_device, Configuration }, _From, State ) ->
	case do_start_device( Configuration ) of
		{ ok, Device } ->
				Devices = orddict:store( Device#device.id, Device, State#state.devices ),
				NewState = State#state{ devices = Devices },
				{ reply, { ok, Device#device.id }, NewState };

		{ error, _ } = Error ->
			{ reply, Error, State }
	end;


handle_call( { stop_device, DeviceId }, _From, State ) ->
	case orddict:find( DeviceId, State#state.devices ) of
		{ ok, Device } ->
			lurch_driver_port:stop_driver( Device#device.port ),
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


handle_call( { poll_device_event, DeviceId, Event }, _From, State ) ->
	Reply = case orddict:find( DeviceId, State#state.devices ) of
		{ ok, Device } ->
			case lists:member( Event, Device#device.events ) of
				true -> { ok, <<"dummy">> };
				false -> { error, no_such_event }
			end;

		error -> { error, no_such_device }
	end,
	{ reply, Reply, State };



handle_call( stop, _From, State ) ->
	{ stop, shutdown, ok, State }.


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

-spec do_start_device( term() ) -> { ok, term() } | { error, term() }.
do_start_device( Configuration ) ->
	Driver = proplists:get_value(driver, Configuration),
	Parameters = proplists:get_value(parameters, Configuration),
	Events = proplists:get_value(events, Configuration, []),
	case lurch_driver_port:start_driver( Driver, Parameters ) of
		{ ok, Port } ->
			DeviceId = make_ref( ),
			Device = #device{ id = DeviceId
							, driver = Driver
							, parameters = Parameters
							, events = Events
							, port = Port },
			{ ok, Device };
		{ error, _ } = Error ->
			Error
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
	, ?setup( test_poll_device_event ) }.


% setup functions
test_start( ) ->
	{ ok, Pid } = start( ),
	meck:new( lurch_driver_port, [ ] ),
	meck:expect( lurch_driver_port, start_driver,
				 fun( _Driver, _Parameters ) -> { ok, port_mock } end ),
	meck:expect( lurch_driver_port, stop_driver,
				 fun( Port ) -> ok end ),
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
	meck:expect( lurch_driver_port, start_driver,
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


% Helper functions
test_poll_device_event( Pid ) ->
	{ ok, DeviceId } = start_device( Pid, dummy_driver_config( ) ),
	InvalidDeviceId = make_ref(),
	InvalidEvent = <<"non_existing_event">>,
	Res1 = poll_device_event( Pid, DeviceId, ?EVENT_NAME ),
	Res2 = poll_device_event( Pid, InvalidDeviceId, ?EVENT_NAME ),
	Res3 = poll_device_event( Pid, DeviceId, InvalidEvent ),
	[ { "poll event", ?_assertEqual( { ok, <<"dummy">> }, Res1 ) }
	, { "bad device", ?_assertEqual( { error, no_such_device }, Res2 ) }
	, { "bad event", ?_assertEqual( { error, no_such_event }, Res3 ) }
	].



dummy_driver_config( ) ->
	[ { driver, ?DRIVER_NAME }
	, { parameters, [] }
	, { events, [ ?EVENT_NAME ] }
	].

-endif. % TEST

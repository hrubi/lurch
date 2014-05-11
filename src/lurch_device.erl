% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from drivers

-module( lurch_device ).

-behaviour( gen_server ).

% API functions
-export(
	[ start/0, start_link/0, stop/1
	, start_device/3, stop_device/2, list_devices/1
	] ).

% gen_server callbacks
-export(
    [ init/1, handle_call/3, handle_cast/2
	, handle_info/2, terminate/2, code_change/3
	] ).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start( ) -> pid().
start( ) ->
	gen_server:start( ?MODULE, [ ], [ ] ).

-spec start_link( ) -> pid().
start_link( ) ->
	gen_server:start_link( ?MODULE, [ ], [ ] ).

-spec stop( pid() ) -> ok.
stop( Server ) ->
	gen_server:call( Server, stop ).

-type device_id() :: reference().

-spec start_device( pid(), binary(), [ binary() ] ) -> { ok, device_id() }.
start_device( Server, Driver, Parameters ) ->
	gen_server:call( Server, { start_device, Driver, Parameters } ).

-spec stop_device( pid(), device_id() ) -> ok.
stop_device( Server, Id ) ->
	gen_server:call( Server, { stop_device, Id } ).

-spec list_devices( pid() ) ->  [ [ proplists:property() ] ].
list_devices( Server ) ->
	gen_server:call( Server, list_devices ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
-record( device,
	{ id         :: device_id()
	, driver     :: binary()
	, parameters :: [ binary() ]
	, port		 :: port()
	} ).

-record( state,
	{ devices :: orddict:orddict()
	} ).

-spec init( term() ) -> { ok, [ term() ]} | { stop, term() }.
init( _Args ) ->
	Devices = orddict:new(),
	State = #state{ devices = Devices },
	{ ok, State }.


handle_call( { start_device, Driver, Parameters }, _From, State ) ->
	case do_start_device( Driver, Parameters ) of
		{ ok, Port } ->
				DeviceId = make_ref( ),
				Device = #device{ driver = Driver
								, parameters = Parameters
								, id = DeviceId
								, port = Port },
				Devices = orddict:store( DeviceId, Device, State#state.devices ),
				NewState = #state{ devices = Devices },
				{ reply, { ok, DeviceId }, NewState };

		{ error, Reason } ->
			{ reply, { error, Reason }, State }
	end;

handle_call( { stop_device, DeviceId }, _From, State ) ->
	do_stop_device( DeviceId ),
	NewState = orddict:erase( DeviceId, State#state.devices ),
	{ reply, ok, NewState };

handle_call( list_devices, _From, State ) ->
	DeviceList = lists:map(
				   fun( { _Key, Val } ) -> device_to_proplist( Val ) end,
				   orddict:to_list( State#state.devices ) ),
	{ reply, { ok, DeviceList }, State };

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

do_start_device( _Driver, _Parameters ) ->
	Port = undefined,
	{ ok, Port }.

do_stop_device( _DeviceId ) ->
	ok.

device_to_proplist( Device ) ->
	[ { id, Device#device.id }
	, { driver, Device#device.driver }
	, { parameters, Device#device.parameters }
	].

%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( setup( F ), { setup, fun test_start/0, fun test_stop/1, F } ).

% Test descriptions
server_test_( ) ->
	{ "Server can be started and stopped"
	, ?setup( fun test_is_alive/1 ) }.

device_start_stop_test_( ) ->
	{ "Device can be started and stopped"
	, ?setup( fun test_start_stop_device/1 ) }.

device_list_test_( ) ->
	{ "Devices can be added and listed"
	, ?setup( fun test_add_list_devices/1 ) }.

% Setup functions
test_start( ) ->
	{ ok, Pid } = start( ),
	Pid.

test_stop( Pid ) ->
	stop( Pid ).

% Actual tests
test_is_alive( Pid ) ->
	[ ?_assert( erlang:is_process_alive( Pid ) ) ].

test_start_stop_device( Server ) ->
	Result1 = start_device( Server, <<"dummy">>, [ ] ),
	Result2 = stop_device( Server, element( 2, Result1 ) ),
	[ ?_assertMatch( { ok, _Ref }, Result1 )
	, ?_assertEqual( ok, Result2 ) ].

test_add_list_devices( Server ) ->
	DeviceCount = 3,
	StartDeviceOk = fun( ) ->
		{ ok, DeviceId } = start_device( Server, <<"dummy">>, [ ] ),
		DeviceId
	end,
	DeviceIds = [ StartDeviceOk( ) || _N <- lists:seq( 1, DeviceCount ) ],
	{ ok, Result } = list_devices( Server ),
	GetDeviceFields = fun( Field, Devices ) ->
		[ proplists:get_value( Field, Device ) || Device <- Devices ]
	end,
	[ ?_assertEqual( DeviceCount, length( Result ) )
	, [ ?_assert( lists:member( Id, DeviceIds ) ) || Id <- GetDeviceFields( id, Result ) ]
	, [ ?_assertEqual( <<"dummy">>, Driver ) || Driver <- GetDeviceFields( driver, Result ) ]
	].


-endif. % TEST

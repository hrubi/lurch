% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_device ).

-behaviour( gen_server ).

% lurch_device callbacks
-callback start_driver( Driver :: term(), Parameters :: [ term() ] ) ->
    { ok | error, Result :: term(), Context :: term() }
    | { error, Result :: term(), Context :: term() }.

-callback get_event( Event :: term(), Context :: term() ) ->
    { ok | error, Result :: term(), NewContext :: term() }.

-callback stop_driver( Reason :: term(), Context :: term() ) ->
    { ok | error , Result :: term(), NewContext :: term() }.

-callback handle_info( Msg :: term(), Context :: term() ) ->
    { ok | error, Result :: term(), NewContext :: term() }.

% Types

-type msg_tag() :: reference().
-type device_tag() :: term().

-export_type( [ msg_tag/0, device_tag/0 ] ).

-type device_impl() :: module().
-type device_id() :: { device_impl(), device_tag() }.
-type driver() :: driver_external().
-type driver_external() :: binary().
% API functions

-export(
    [ start/4
    , stop/1
    , request_event/2
    ] ).

% Internal API
-export( [ start_link/5 ] ).

% gen_server callbacks
-export(
    [ init/1, handle_call/3, handle_cast/2
    , handle_info/2, terminate/2, code_change/3
    ] ).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start( Sup :: pid(),
             DriverSpec :: driver(), Parameters :: [ term() ], Owner :: pid() ) ->
    { ok, device_id() } | { error, term() }.
start( Sup, Driver, Parameters, Owner ) ->
    DevId = make_ref(),
    DevImpl = driver_impl( Driver ),
    StartFun = start_link,
    StartArgs = [ DevId, DevImpl, Owner, Driver, Parameters ],
    % FIXME - client would probably like to monitor the supervisor as well,
    %         to know when the device has finally crashed totally and won't
    %         be restarted.
    { ok, _ } = supervisor:start_child( Sup, [ ?MODULE, StartFun, StartArgs ] ),
    { ok, DevId }.

-spec stop( Id :: device_id() ) ->
    ok | { error | term() }.
stop( Id ) ->
    gen_server:cast( lurch_proc:via( Id ), stop ).

-spec request_event( Id :: device_id(), Event :: term() ) ->
    { ok, msg_tag() } | { error, term() }.
request_event( Id, Event ) ->
    Tag = make_ref(),
    ok = gen_server:cast( lurch_proc:via( Id ), { get_event, Event, Tag } ),
    { ok, Tag }.

%% ===================================================================
%% Internal API
%% ===================================================================
start_link( Id, Impl, Owner, Driver, Parameters ) ->
    gen_server:start_link( ?MODULE, { Id, Impl, Owner, Driver, Parameters }, [] ).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

-record( state,
    { id :: term()
    , impl :: pid()
    , owner :: pid()
    , context :: term() | undefined
    } ).

init( { Id, Impl, Owner, Driver, Params } ) ->
    process_flag( trap_exit, true ),
    true = lurch_proc:reg( Id ),
    self() ! { start_driver, Driver, Params },
    { ok, #state{ id = Id, impl = Impl, owner = Owner } }.


handle_call( _Msg, _From, State ) ->
    { stop, { error, unsupported_call }, State }.

handle_cast( { get_event, Event, Tag }, State ) ->
    call_impl_ctx( get_event, [ Event ], State, Tag );

handle_cast( stop, State ) ->
    { stop, shutdown, State }.


handle_info( { start_driver, Driver, Params }, State ) ->
    call_impl( start_driver, [ Driver, Params ], State );

handle_info ( Msg, State ) ->
    call_impl_ctx( handle_info, [ Msg ], State ).


terminate( Reason, State ) ->
    call_impl_ctx( stop_driver, [ Reason ], State ),
    ok.


code_change( _OldVsn, State, _Extra ) ->
    % FIXME - call the implementation's code_change
    { ok, State }.


%% ===================================================================
%% Internal functions
%% ===================================================================

driver_impl( Driver )
    when is_binary( Driver ) or
         is_list( Driver )
    -> lurch_device_ext.

call_impl_ctx( Func, Params, State ) ->
    call_impl_ctx( Func, Params, State, State#state.id ).

call_impl_ctx( Func, Params, State, Tag ) ->
    Params2 =  Params ++ [ State#state.context ],
    call_impl( Func, Params2, State, Tag ).

call_impl( Func, Params, State ) ->
    call_impl( Func, Params, State, State#state.id ).

call_impl( Func, Params, State, Tag ) ->
    Result = erlang:apply( State#state.impl, Func, Params ),
    Action = impl_func_to_action_name( Func ),
    handle_impl_result( Result, Action, State, Tag ).

handle_impl_result( { ok, Result, Context }, Action, State, Tag ) ->
    NewState = State#state{ context = Context },
    send_owner( NewState, Action, { ok, Result }, Tag ),
    { noreply, NewState };

handle_impl_result( { error, Result, Context }, Action, State, Tag ) ->
    NewState = State#state{ context = Context },
    send_owner( State, Action, { error, Result }, Tag ),
    { stop, Result, NewState }.

impl_func_to_action_name( start_driver ) -> start;
impl_func_to_action_name( stop_driver ) -> stop;
impl_func_to_action_name( get_event ) -> event.

send_owner( State, Event, Msg, Id ) ->
    State#state.owner ! { Event, Msg, Id }.


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

-define( IMPL, lurch_device_impl ).


impl_test_() ->
    { foreach,
      fun mock_impl_module/0,
      fun mock_impl_module_stop/1,
      [ fun test_start_device/1
      , fun test_get_event/1
      %, fun test_stop_device/1
      ]
    }.

test_start_device( S0 ) ->
    R1 = handle_info( { start_driver, good_driver, [ params ] }, S0 ),
    R2 = handle_info( { start_driver, bad_driver, [ params ] }, S0 ),
    % FIXME - test the side effects (send to owner)
    [ ?_assertMatch( { noreply, #state{ context = context1 } }, R1 )
    , ?_assertMatch( { stop, crashed, #state{ context = context2 } }, R2 )
    ].

test_get_event( S0 ) ->
    Tag = make_ref(),
    R1 = handle_cast( { get_event, good_event, Tag }, S0 ),
    R2 = handle_cast( { get_event, bad_event, Tag }, S0 ),
    % FIXME - test the side effects (send to owner)
    [ ?_assertMatch( { noreply, #state{ context = context1 } }, R1 )
    , ?_assertMatch( { stop, reason, #state{ context = context2 } }, R2 )
    ].


mock_impl_module() ->
    meck:new( ?IMPL, [ non_strict ] ),
    meck:expect( ?IMPL, start_driver,
        fun( good_driver, _Parameters ) -> { ok, started, context1 };
           ( bad_driver, _Parameters ) -> { error, crashed, context2 }
        end ),
    meck:expect( ?IMPL, get_event,
        fun( good_event, _Ctx ) -> { ok, value, context1 };
           ( bad_event, _Ctx ) -> { error, reason, context2 }
        end ),
    meck:expect( ?IMPL, stop_driver, fun( _, _ ) -> ok end ),

    #state{ id = make_ref(), impl = ?IMPL, owner = self() }.

mock_impl_module_stop( _ ) ->
    meck:unload( ?IMPL ).

-endif. % TEST

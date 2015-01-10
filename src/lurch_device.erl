% @copyright 2014, Jan Hruban
%
% @doc Provides communication mechanism to/from external driver

-module( lurch_device ).

-export_type( [ msg_tag/0, device_tag/0 ] ).

-type msg_tag() :: reference().
-type device_tag() :: term().
-type device_impl() :: module().
-type device_id() :: { device_impl(), device_tag() }.
-type driver() :: driver_external().
-type driver_external() :: binary().

-callback start_link( Id :: device_tag(), Driver :: term(), Parameters :: [ term() ],
                      Owner :: pid() ) ->
    { ok, pid() } | { error, term() }.

-callback stop( Id :: device_tag() ) -> ok | { error, term() }.

-callback request_event( Id :: device_tag(), Event :: term() ) ->
    { ok, msg_tag() } | { error, term() }.


-export(
    [ start/4
    , stop/1
    , request_event/2
    ] ).

-spec start( Sup :: pid(),
             DriverSpec :: driver(), Parameters :: [ term() ], Owner :: pid() ) ->
    { ok, device_id() } | { error, term() }.
start( Sup, Driver, Parameters, Owner ) ->
    DevTag = make_ref(),
    DevImpl = driver_impl( Driver ),
    DevId = { DevImpl, DevTag },
    StartFun = start_link,
    StartArgs = [ DevId, Driver, Parameters, Owner ],
    { ok, _ } = supervisor:start_child( Sup, [ DevImpl, StartFun, StartArgs ] ),
    { ok, DevId }.

-spec stop( Id :: device_id() ) ->
    ok | { error | term() }.
stop( { Impl, _Tag } = Id ) ->
    Impl:stop( Id ).

-spec request_event( Id :: device_id(), Event :: term() ) ->
    { ok, msg_tag() } | { error, term() }.
request_event( { _DevSup, { Impl, DevId } }, Event ) ->
    Impl:request_event( DevId, Event ).

% FIXME - this is hack, explicitly distinguish the driver type.
driver_impl( Driver )
    when is_binary( Driver ) or
         is_list( Driver )
    -> lurch_device_ext.

% @copyright 2014, Jan Hruban
%
% @doc Top level supervisor

-module( lurch_sup ).

-behaviour( supervisor ).

%% API
-export( [ start_link/0 ] ).

%% supervisor callbacks
-export( [ init/1 ] ).


%% ===================================================================
%% API
%% ===================================================================
start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================
init( [] ) ->
    Flags = { one_for_one, 5, 10 },
    Workers = [ worker_spec( lurch_devman, [ start_dev_sup_fun() ], [ gen_server ] ) ],
    { ok, { Flags, Workers } }.


%% ===================================================================
%% Internal functions
%% ===================================================================

worker_spec( Name, Args, Modules ) ->
    { Name, { Name, start_link, Args }
    , permanent, timer:seconds( 5 ), worker, [ Name ] ++ Modules }.

supervisor_spec( Name, Args ) ->
    { Name, { Name, start_link, Args }
    , permanent, infinity, supervisor, [ Name, supervisor ] }.

start_dev_sup_fun() ->
    Sup = self(),
    WorkerSpec = supervisor_spec( lurch_dev_sup, [ main ] ),
    fun() -> supervisor:start_child( Sup, WorkerSpec ) end.

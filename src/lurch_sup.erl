% @copyright 2014, Jan Hruban
%
% @doc Top level supervisor

-module( lurch_sup ).

-behaviour( supervisor ).

%% API
-export(
    [ start_link/0
    , start_devman/0
    ] ).

%% supervisor callbacks
-export( [ init/1 ] ).

-define( CHILD( I, Type, Args ),
        { I
        , { I, start_link, Args }
        , permanent
        , 5000
        , Type
        , [I]
        } ).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================
start_link() ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

start_devman() ->
    supervisor:start_child( ?MODULE, dev_sup_spec() ),
    supervisor:start_child( ?MODULE, devman_spec() ).


% supervisor callbacks

init( [] ) ->
    { ok, { { one_for_one, 5, 10 }, [] }  }.


%% ===================================================================
%% Internal functions
%% ===================================================================
devman_spec() ->
    ?CHILD( lurch_devman, worker, [] ).

dev_sup_spec() ->
    ?CHILD( lurch_dev_sup, supervisor, [ main ] ).

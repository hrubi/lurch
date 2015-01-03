% @copyright 2014, Jan Hruban
%
% @doc Supervising devices.
% There are two levels of supervision, the main one is permanent and the
% intermediate level is temporary so in case the intermediate supervisor dies
% due to it's child dying too often, the upper supervisor does not crash too.

-module( lurch_dev_sup ).

-behaviour( supervisor ).

%% API
-export(
    [ start_link/1
    , start_link/2
    , start_dev/4
    ] ).

%% supervisor callbacks
-export( [ init/1 ] ).

%% ===================================================================
%% API
%% ===================================================================
start_link( main ) ->
    supervisor:start_link( ?MODULE, main ).

start_link( intermediate, Args ) ->
    supervisor:start_link( ?MODULE, { intermediate, Args } ).

% FIXME - ugly, those parameters should not be exposed here
start_dev( Sup, Id, Driver, Parameters ) ->
    Caller = self(),
    supervisor:start_child( Sup, [ [ Id, Driver, Parameters, Caller ] ] ).

% supervisor callbacks

init( main ) ->
    { ok, { { simple_one_for_one, 5, 10 }, [ intermediate_spec() ] } };

init( { intermediate, Args } ) ->
    { ok, { { one_for_one, 5, 10 }, [ lurch_dev_spec( Args ) ] }  }.

% internal
intermediate_spec() ->
    { any
    , { ?MODULE, start_link, [ intermediate ] }
    , temporary
    , 5000
    , supervisor
    , [ ?MODULE ]
    }.

lurch_dev_spec( Args ) ->
    { any
    , { lurch_dev, start_link, Args }
    , transient
    , 5000
    , worker
    , [ lurch_dev  ]
    }.

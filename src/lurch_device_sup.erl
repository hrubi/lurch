% @copyright 2014, Jan Hruban
%
% @doc Supervising devices.
% There are two levels of supervision, the main one is permanent and the
% intermediate level is temporary so in case the intermediate supervisor dies
% due to it's child dying too often, the upper supervisor does not crash too.

-module( lurch_device_sup ).

-behaviour( supervisor ).

%% API
-export(
    [ start_link/1
    , start_link/4
    ] ).

%% supervisor callbacks
-export( [ init/1 ] ).

%% ===================================================================
%% API
%% ===================================================================
start_link( main ) ->
    supervisor:start_link( ?MODULE, main ).

start_link( intermediate, M, F, A ) ->
    supervisor:start_link( ?MODULE, { intermediate, M, F, A } ).

% supervisor callbacks

init( main ) ->
    { ok, { { simple_one_for_one, 5, 10 }, [ intermediate_spec() ] } };

init( { intermediate, M, F, A } ) ->
    { ok, { { one_for_one, 5, 10 }, [ lurch_device_spec( M, F, A ) ] }  }.

% internal
intermediate_spec() ->
    { any
    , { ?MODULE, start_link, [ intermediate ] }
    , temporary
    , 5000
    , supervisor
    , [ ?MODULE ]
    }.

lurch_device_spec( M, F, A ) ->
    { any
    , { M, F, A }
    , transient
    , 5000
    , worker
    , [ lurch_device  ]
    }.

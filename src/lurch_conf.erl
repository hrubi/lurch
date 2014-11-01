% @copyright 2014, Jan Hruban
%
% @doc Reads various configuration files

-module( lurch_conf ).

-define( CONF_DIR, code:lib_dir( lurch, conf ) ).

-export(
    [ read_devices/1
	] ).

-spec read_devices( file:name_all() ) -> list( proplists:property() ).
read_devices( Name ) ->
    read_conf( Name, fun parse_devices/1 ).

% Internal functions

read_conf( Name, Fun ) ->
    Fun( from_file( conf_file( Name ) ) ).

conf_file( Name ) ->
    case lurch_os:safe_relative_path( Name ) of
        undefined -> throw( { unsafe_relative_path, Name } );
        Path ->
            filename:join( [ ?CONF_DIR, Path ] ) ++ ".json"
    end.

from_file( Name ) ->
	{ ok, Content } = file:read_file( Name ),
	jiffy:decode( Content ).

parse_devices( Devices ) ->
    { [ { <<"devices">>, DeviceList } ] } = Devices,
    lists:map( fun parse_device/1, DeviceList ).

parse_device( Device ) ->
    { [ { Name, { Opts } } ] } = Device,
    [ { <<"name">>, Name } | Opts ].


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

read_devices_test_() ->
    [ D ] = read_devices( test_conf_file( "devices" ) ),
    io:format( user, "~p~n", [ D ] ),
    [ ?_assertEqual( <<"test/random.sh">>,
                     proplists:get_value( <<"driver">>, D ) )
    , ?_assertEqual( [], proplists:get_value( <<"events">>, D ) )
    , ?_assertEqual( <<"sample">>, proplists:get_value( <<"name">>, D ) )
    , ?_assertEqual( [], proplists:get_value( <<"parameters">>, D ) )
    ].


% Helper functions
test_conf_file( Name ) ->
    filename:join( [ test, Name ] ).

-endif. % TEST

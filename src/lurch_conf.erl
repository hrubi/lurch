% @copyright 2014, Jan Hruban
%
% @doc Reads various configuration files

-module( lurch_conf ).

-define( CONF_DIR, code:lib_dir( lurch, conf ) ).

-export(
    [ read_devices/1
	] ).

% API functions

-spec read_devices( file:name_all() ) -> list( list( proplists:property() ) ).
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
    key_to_atom(
        value_to_string(
            [ { name, Name } | Opts ]
    ) ).

key_to_atom( Proplist ) ->
    lists:map(
        fun
            ( { K, V } ) when is_binary( K ) -> { binary_to_atom( K, utf8 ), V };
            ( Other ) -> Other
        end,
        Proplist
     ).

value_to_string( PropList ) ->
    lists:map(
        fun
            ( { K, V } ) when is_binary( V ) -> { K, binary_to_list( V ) };
            ( Other ) -> Other
        end,
        PropList
    ).


%% ===================================================================
%% Tests
%% ===================================================================
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).

read_devices_test_() ->
    [ D ] = read_devices( test_conf_file( "devices" ) ),
    [ ?_assertEqual( "test/echo.sh",
                     proplists:get_value( driver, D ) )
    , ?_assertEqual( [], proplists:get_value( events, D ) )
    , ?_assertEqual( "sample", proplists:get_value( name, D ) )
    , ?_assertEqual( [], proplists:get_value( parameters, D ) )
    ].

key_to_atom_test_() ->
    L = [ { <<"bin">>, 1 }, 1, { 1, 2 }, { 1, 2, 3 } ],
    Exp = [ { bin, 1 }, 1, { 1, 2 }, { 1, 2, 3 } ],
    Res = key_to_atom( L ),
    [ ?_assertEqual( Exp, Res ) ].

value_to_string_test_() ->
    L = [ { 1, <<"bin">> }, 1, { 1, 2 }, { 1, 2, 3 } ],
    Exp = [ { 1, "bin" }, 1, { 1, 2 }, { 1, 2, 3 } ],
    Res = value_to_string( L ),
    [ ?_assertEqual( Exp, Res ) ].


% Helper functions
test_conf_file( Name ) ->
    filename:join( [ test, Name ] ).

-endif. % TEST

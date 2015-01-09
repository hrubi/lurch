-module( lurch_mock ).

-ifdef( TEST ).

-export(
    [ module/3
    , module_stop/1
    , lurch_os_safe_relative_path/0
    ] ).

module( Module, Function, Fun ) ->
    meck:new( Module, [ passthrough ] ),
    meck:expect( Module, Function, Fun ),
    Module.

module_stop( Module ) ->
    meck:unload( Module ).


lurch_os_safe_relative_path() ->
    module( lurch_os, safe_relative_path,
        fun ( safe ) -> "safe";
            ( unsafe ) -> undefined
        end ).

-endif. % TEST

-module(lua_module).

-export([behaviour_info/1]).

%% @private
-spec behaviour_info(_) -> undefined | [{lua_call, 1}, ...].
behaviour_info(callbacks) ->
	[{lua_call, 1}];
behaviour_info(_Other) ->
	undefined.

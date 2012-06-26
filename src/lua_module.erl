-module(lua_module).

-export([behaviour_info/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @private
-spec behaviour_info(_) -> undefined | [{lua_call, 1}, ...].
behaviour_info(callbacks) ->
	[{lua_call, 1}];
behaviour_info(_Other) ->
	undefined.

-ifdef(TEST).

behaviour_info_test_() ->
	[
        ?_assertEqual(behaviour_info(callbacks), [{lua_call, 1}]),
        ?_assertEqual(behaviour_info(something), undefined)
	].

-endif.
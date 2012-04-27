-module(nodelua).

-export([run/1, send/2]).
-export([run_core/1, send_core/2]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
  PrivDir = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                    EbinDir = filename:dirname(code:which(?MODULE)),
                    AppPath = filename:dirname(EbinDir),
                    filename:join(AppPath, "priv");
                Path ->
                    Path
            end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

run(Script) ->
    run_core(Script).

send(Lua, Message) ->
    send_core(Lua, Message).

run_core(_Script) ->
    ?nif_stub.

send_core(_Ref, _Message) ->
    ?nif_stub.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Script} = file:read_file("../scripts/basic_test.lua"),
    {ok, Ref} = run(Script),
    ?assertEqual(ok, send(Ref, ok)).

message_send_test() ->
    {ok, Script} = file:read_file("../scripts/incoming_message.lua"),
    {ok, Ref} = run(Script),
    ?assertEqual(ok, send(Ref, ok)),
    ?assertEqual(ok, send(Ref, <<"mkay">>)),
    ?assertEqual(ok, send(Ref, [])),
    ?assertEqual(ok, send(Ref, [ok, {something, foobar}, {{type, point}, {x, 1}, {y, 2}}])),
    ?assertEqual(ok, send(Ref, 2)),
    ?assertEqual(ok, send(Ref, -2)),
    ?assertEqual(ok, send(Ref, -0.2)),
    ?assertEqual(ok, send(Ref, 0.2)),
    ?assertEqual(ok, send(Ref, fun(A) -> A end)),
    ?assertEqual(ok, send(Ref, self())),
    ?assertEqual(ok, send(Ref, Ref)),
    ?assertEqual(ok, send(Ref, {ok, tuple, {something, other}, bla})),
    receive after 1000 -> true end.

-endif.

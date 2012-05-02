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

basic_tes() ->
    {ok, Script} = file:read_file("../scripts/basic_test.lua"),
    {ok, Ref} = run(Script),
    ?assertEqual(ok, send(Ref, ok)).

bounce_message(Ref, Message, Expected) ->
    send(Ref, [{pid, self()}, {message, Message}]),
    receive 
        Response -> 
            ?assertEqual(Expected, Response)
    end.

translation_test() ->
    {ok, Script} = file:read_file("../scripts/incoming_message.lua"),
    {ok, Ref} = run(Script),
    bounce_message(Ref, ok, <<"ok">>),
    bounce_message(Ref, <<"mkay">>, <<"mkay">>),
    bounce_message(Ref, [], []),
    bounce_message(Ref, 2, 2.0),
    bounce_message(Ref, -2, -2.0),
    bounce_message(Ref, -0.2, -0.2),
    bounce_message(Ref, 0.2, 0.2),
    bounce_message(Ref, fun(A) -> A end, <<"sending a function reference is not supported">>),
    MakeRefValue = erlang:make_ref(), % this may not work if the erlang environment is cleared
    bounce_message(Ref, MakeRefValue, MakeRefValue),
    bounce_message(Ref, Ref, Ref),
    bounce_message(Ref, [ok], [{1.0, <<"ok">>}]),
    bounce_message(Ref, true, true),
    bounce_message(Ref, false, false),
    bounce_message(Ref, nil, nil),
    bounce_message(Ref, "test", <<"test">>).


-endif.

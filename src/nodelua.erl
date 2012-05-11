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
    NumProcessors = erlang:system_info(logical_processors),
    erlang:load_nif(filename:join(PrivDir, ?MODULE), NumProcessors).

run(Script) ->
    run_core(Script).

send(Lua, Message) ->
    send_core(Lua, [{pid, self()}, {data, Message}]).

run_core(_Script) ->
    ?nif_stub.

send_core(_Ref, _Message) ->
    ?nif_stub.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Script} = file:read_file("../test_scripts/basic_test.lua"),
    {ok, Ref} = run(Script),
    ?assertEqual(ok, send(Ref, ok)).

bounce_message(Ref, Message, Expected) ->
    send(Ref, Message),
    receive 
        Response -> 
            ?assertEqual(Expected, Response)
    end.

translation_test() ->
    {ok, Script} = file:read_file("../test_scripts/incoming_message.lua"),
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
    bounce_message(Ref, [{1, <<"ok">>}], [{1.0, <<"ok">>}]),
    bounce_message(Ref, [{3, 3}, {2, 2}, one, four, five], [{1.0,<<"one">>}, {2.0,2.0}, {3.0,3.0}, {4.0,<<"four">>}, {5.0,<<"five">>}]),
    bounce_message(Ref, {{3, 3}, {2, 2}, one, four, five}, [{1.0,<<"one">>}, {2.0,2.0}, {3.0,3.0}, {4.0,<<"four">>}, {5.0,<<"five">>}]),
    bounce_message(Ref, [first, {1, <<"ok">>}], [{1.0,<<"ok">>},{2.0,<<"first">>}]),
    bounce_message(Ref, {}, []),
    bounce_message(Ref, [{ok, ok}], [{<<"ok">>,<<"ok">>}]),
    % instead of clobbering KvP with matching K, append them as a list, not awesome, but doesn't lose data
    bounce_message(Ref, [{ok, ok}, {ok, notok}], [{<<"ok">>,<<"ok">>}, {1.0,[{1.0,<<"ok">>},{2.0,<<"notok">>}]}]),
    bounce_message(Ref, [{one, ok}, {two, ok}], [{<<"one">>,<<"ok">>},{<<"two">>,<<"ok">>}]),
    % strings are lists and get treated as such
    bounce_message(Ref, "test", [{1.0,116.0}, {2.0,101.0}, {3.0,115.0}, {4.0,116.0}]).

performance_messages(Ref) ->
    [ send(Ref, X) || X <- lists:seq(1, 100000) ],
    [ receive Y -> Z = erlang:trunc(Y), ?assertEqual(X, Z) end || X <- lists:seq(1, 10000) ].
performance_test() ->
    {ok, Script} = file:read_file("../test_scripts/performance.lua"),
    {ok, Ref} = run(Script),
    {Time, _} = timer:tc(fun performance_messages/1, [Ref]),
    % have to keep a referenco to Ref otherwise it will be
    % garbage collected half way through processing
    io_lib:format("~p took ~p to process~n", [Ref, Time]),
    erlang:display(Time).

-endif.

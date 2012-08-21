-module(nlua).

-export([start/2]).
-export([load/2, send/2]).
-export([load_core/2, send_core/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-on_load(init/0).

% This interface is intended to be a simple bridge to lua, no more.
start(_StartType, _StartArgs) ->
    Script = config(script, undefined),
    {ok, Pid} = nodelua:start_link(Script), 
    {ok, Pid, []}.

config(Name, Default) ->
    case application:get_env(?MODULE, Name) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

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

load(Script, OwnerPid) ->
    load_core(Script, OwnerPid).

send(Lua, Message) ->
    send_core(Lua, Message).

load_core(_Script, _OwnerPid) ->
    ?nif_stub.

send_core(_Ref, _Message) ->
    ?nif_stub.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

crash1_test() ->
    [ basic_test() || _ <- lists:seq(1, 100) ].

basic_test() ->
    {ok, Script} = file:read_file("../test_scripts/basic_test.lua"),
    {ok, Ref} = ?MODULE:load(Script, self()),
    ?assertEqual(ok, ?MODULE:send(Ref, ok)).

bounce_message(Ref, Message) ->
    ?MODULE:send(Ref, { {data, Message}, {pid, self()} }),
    receive 
        Response -> 
            Response
    end.

translation_test_() ->
    {ok, Script} = file:read_file("../test_scripts/incoming_message.lua"),
    {ok, Ref} = ?MODULE:load(Script, self()),
    MakeRefValue = erlang:make_ref(), % this may not work if the erlang environment is cleared
    [
        ?_assertEqual(bounce_message(Ref, ok), <<"ok">>),
        ?_assertEqual(bounce_message(Ref, <<"mkay">>), <<"mkay">>),
        ?_assertEqual(bounce_message(Ref, []), []),
        ?_assertEqual(bounce_message(Ref, 2), 2),
        ?_assertEqual(bounce_message(Ref, -2), -2),
        ?_assertEqual(bounce_message(Ref, -0.2), -0.2),
        ?_assertEqual(bounce_message(Ref, 0.2), 0.2),
        ?_assertEqual(bounce_message(Ref, fun(A) -> A end), <<"sending a function reference is not supported">>),
        ?_assertEqual(bounce_message(Ref, MakeRefValue), MakeRefValue),
        ?_assertEqual(bounce_message(Ref, Ref), Ref),
        ?_assertEqual(bounce_message(Ref, [ok]), [<<"ok">>]),
        ?_assertEqual(bounce_message(Ref, true), true),
        ?_assertEqual(bounce_message(Ref, false), false),
        ?_assertEqual(bounce_message(Ref, nil), nil),
        ?_assertEqual(bounce_message(Ref, [{1, <<"ok">>}]), [<<"ok">>]),
        ?_assertEqual(bounce_message(Ref, [{3, 3}, {2, 2}, one, four, five]), [<<"one">>, 2, 3, <<"four">>, <<"five">>]),
        ?_assertEqual(bounce_message(Ref, {{3, 3}, {2, 2}, one, four, five}), [<<"one">>, 2, 3, <<"four">>, <<"five">>]),
        ?_assertEqual(bounce_message(Ref, [first, {1, <<"ok">>}]), [<<"ok">>,<<"first">>]),
        ?_assertEqual(bounce_message(Ref, {}), []),
        ?_assertEqual(bounce_message(Ref, [{ok, ok}]), [{<<"ok">>,<<"ok">>}]),
        % instead of clobbering KvP with matching K, append them as a list, not awesome, but doesn't lose data
        ?_assertEqual(lists:keysort(1, bounce_message(Ref, [{ok, ok}, {ok, notok}])), [{1,[<<"ok">>,<<"notok">>]},{<<"ok">>,<<"ok">>}]),
        ?_assertEqual(lists:keysort(1, bounce_message(Ref, [{one, ok}, {two, ok}])), [{<<"one">>,<<"ok">>},{<<"two">>,<<"ok">>}]),
        % strings are lists and get treated as such
        ?_assertEqual(bounce_message(Ref, "test"), [116, 101, 115, 116])
    ]
    .

performance_messages(Ref) ->
    PidTuple = {pid, self()},
    [ ?MODULE:send(Ref, { {data, X}, PidTuple }) || X <- lists:seq(1, 10000) ],
    [ receive Y -> Z = erlang:trunc(Y), ?assertEqual(X, Z) end || X <- lists:seq(1, 10000) ].
performance_test() ->
    {ok, Script} = file:read_file("../test_scripts/performance.lua"),
    {ok, Ref} = ?MODULE:load(Script, self()),
    ?debugTime("performance_test", timer:tc(fun performance_messages/1, [Ref])),
    % have to keep a referenco to Ref otherwise it will be
    % garbage collected half way through processing
    io_lib:format("~p processed~n", [Ref]).

owner_pid_test() ->
    MyPid = self(),
    {ok, Script} = file:read_file("../test_scripts/owner_pid.lua"),
    {ok, Ref} = ?MODULE:load(Script, MyPid),
    ?MODULE:send(Ref, ok),
    receive
        Data -> ?assertEqual(MyPid, Data)
    end.


-endif.

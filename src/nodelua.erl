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
    %{ok, Ref} = run("print(os.getenv('PWD'))"),
    %{ok, Ref} = run("print(package.path)"),
    {ok, Ref} = run(Script),
    ?assertEqual(ok, send(Ref, ok)).

-endif.

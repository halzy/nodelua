-module(nodelua).

-export([run/1, send/2]).

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

run(_Script) ->
    ?nif_stub.

send(_Ref, _Message) ->
    ?nif_stub.


%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = run("print('hello world')"),
    ?assertEqual(ok, send(Ref, ok)).

-endif.

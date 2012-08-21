-module(nodelua_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).

start(_StartType, _StartArgs) ->
	nodelua_sup:start_link().

stop(_State) ->
	ok.

start() -> start(nodelua).
start(App) ->
    start_ok(App, application:start(App, permanent)).
start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) -> 
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) -> 
    erlang:error({app_start_failed, App, Reason}).

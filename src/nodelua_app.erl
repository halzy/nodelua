% Copyright (c) 2012 Benjamin Halsted <bhalsted@gmail.com>
% 
% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the"Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:
% 
% The above copyright notice and this permission notice shall be included
% in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
% OTHER DEALINGS IN THE SOFTWARE.

%% @private
-module(nodelua_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API.

config(Name, Default) ->
    {ok, ApplicationName} = application:get_application(),
    case application:get_env(ApplicationName, Name) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

-spec start(atom(), application:restart_type()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
	StartResponse = nodelua_sup:start_link(),
    start_scripts(config(scripts, [])),
    StartResponse.

start_scripts([{script, Parameters}|Tail]) ->
    Name = proplists:get_value(name, Parameters),
    File = proplists:get_value(file, Parameters),
    Args = proplists:get_value(args, Parameters),
    {ok, _Pid} = nodelua:start_script(Name, File, Args),
    start_scripts(Tail);
start_scripts([]) ->
    ok.

-spec stop(atom()) -> ok | {error, term()}.
stop(_State) ->
	ok.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

-define(APPLICATION, nodelua).

app_start_test() ->
    application:set_env(?APPLICATION, scripts, 
                [{script, [
                    {name, main_script}, 
                    {file, "../scripts/main.lua"}, 
                    {args, [{path, [<<"../test_scripts">>]}, {module, <<"app_start">>}]}
                ]}]),
    nodelua:start(), 
    {ok, Pid} = nodelua:lookup_script(main_script),
    nodelua:set_owner(Pid, self()),
    nodelua:send(Pid, ok),

    receive
        Data -> ?assertEqual({lua_message,Pid,<<"ok">>}, Data)
    end,

    nodelua:stop_script(main_script),
    nodelua:stop().
    
-endif.


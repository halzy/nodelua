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

-module(nodelua).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0]).
-export([start_script/2]).
-export([stop_script/1]).
-export([boot/3]).
-export([send/2]).
-export([reply/2]).
-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-ifdef(TEST).
-export([callback_test_process/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	lua :: nlua:lua_ref(),
    owner :: pid()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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


-spec start_script(any(), nonempty_string()) -> {ok, pid()}.
start_script(Ref, LuaScript) ->
    supervisor:start_child(nodelua_sup, 
        {{?MODULE, Ref}, {?MODULE, start_link, [LuaScript, self()]}, 
            permanent, 5000, worker, [?MODULE]}).

-spec stop_script(any()) -> ok | {error, not_found}.
stop_script(Ref) ->
    case supervisor:terminate_child(nodelua_sup, {?MODULE, Ref}) of
        ok ->
            supervisor:delete_child(nodelua_sup, {?MODULE, Ref});
        {error, Reason} ->
            {error, Reason}
    end.

-spec start_link(nonempty_string(), pid()) -> {ok, pid()}.
start_link(LuaScript, Owner) ->
    gen_server:start_link(?MODULE, [{script, LuaScript}, {owner, Owner}], []).

-spec send(pid(), term()) -> {ok, reference()} | {error, string()}.
send(Pid, Message) ->
	CallToken = erlang:make_ref(),
	ok = gen_server:cast(Pid, {send, self(), CallToken, Message}),
	{ok, CallToken}.

-spec boot(pid(), [binary()], binary()) -> ok.
boot(Pid, Path, Module) ->
	gen_server:call(Pid, {boot, Path, Module}).

%-spec send(lua_ref(), [{type | socket | port | event | data | pid | callback_id | reply, any()}]) -> ok | {error, string()}.
reply([Lua,CallbackId], Response) ->
    nlua:send(Lua, [{type, reply}, {pid, self()}, {callback_id, CallbackId}, {reply, Response}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{script, LuaScript}, {owner, Owner}]) ->
	case file:read_file(LuaScript) of
        {ok, Script} ->
            {ok, LuaReference} = nlua:load(Script, self()),
            {ok, #state{lua=LuaReference, owner=Owner}};
        {error, Message} ->
            {stop, Message}
    end.

handle_call({boot, Path, Module}, _From, State) ->
	CallToken = erlang:make_ref(),
    nlua:send( State#state.lua, [{type, boot}, {token, CallToken}, {path, Path}, {module, Module}]),
    receive
    	[CallToken,[{<<"error">>,Message}]] ->
            lager:warning("Script Error:~n~p~n", [binary_to_list(Message)]),
    		{reply, {error, Message}, State};
    	[CallToken] -> 
    		{reply, ok, State}
    end;
handle_call(stop, _From, State) -> 
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    lager:error("nodelua:handle_call(~p) called!", [Request]),
    {reply, undefined, State}.


handle_cast({send, _From, CallToken, Message}, State) ->
	nlua:send( State#state.lua, [{type, mail}, {pid, self()}, {token, CallToken}, {data, Message}]),
	{noreply, State};
handle_cast(Msg, State) ->
    lager:error("nodelua:handle_cast(~p) called!", [Msg]),
    {noreply, State}.

%% lua can only send messages, we recognize this one
handle_info([<<"invoke">>,ModuleName,Args], State) ->
	ModulePrefix = <<"nlua_">>,
	LuaModule = << ModulePrefix/binary, ModuleName/binary >>,
	Registered = [ erlang:atom_to_binary(Reg, latin1) || Reg <- erlang:registered() ],
	IsModule = lists:member(LuaModule, Registered),
	case IsModule of
		true ->
			Module = binary_to_existing_atom(LuaModule, latin1),
			erlang:apply(Module, lua_call, [Args]);
		false ->
			ok
	end,
	{noreply, State};
%% unrecognized messages from lua are passed to the 'owner'.
handle_info(Info, State) ->
    State#state.owner ! Info,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

setup() -> 
    ?MODULE:start(),
    {ok,Pid} = ?MODULE:start_script(test_nodelua, "../scripts/main.lua"),
    Pid.
    
cleanup(_Pid) ->
    ?MODULE:stop_script(test_nodelua).

main_test_() ->
    {foreach,
		fun setup/0,
		fun cleanup/1,
		[
			fun run_boot_error/1,
			fun run_callback/1,
            fun run_bogus_call/1,
            fun run_bogus_cast/1
        ]
	}.

run_bogus_call(Pid) ->
    ?_assertEqual(gen_server:call(Pid, bla), undefined).

run_bogus_cast(Pid) ->
    ?_assertEqual(gen_server:cast(Pid, bla), ok).

run_boot_error(Pid) ->
	{error, <<Error:5/binary, _Message/binary>>} = boot(Pid, [<<"../test_scripts">>], <<"boot_error">>),
    ?_assertEqual( <<"error">>, Error ).	

callback_test_process(Pid) ->
    receive
        die -> ok;
        Message ->
            {_, Sender} = lists:keyfind(<<"sender">>, 1, Message),
            ?MODULE:reply(Sender, [{pid, Pid}]),
            callback_test_process(Pid)
    end.

run_callback(LuaPid) ->
	ok = ?MODULE:boot(LuaPid, [<<"../scripts/libs">>,<<"../test_scripts">>,<<"../test_scripts/callback_test">>], <<"callback_test">>),
    EchoPid = spawn(?MODULE, callback_test_process, [self()]),
	{ok, _} = ?MODULE:send(LuaPid, [{echo, EchoPid}]),
    Result = receive
       Data -> 
            Data
    end,
    EchoPid ! die,
    ?_assertEqual( <<"async-test">>, Result).

test_unsupported_test_() ->
    [
        ?_assertEqual(code_change(bla, state, bla), {ok, state})
    ].

-endif.

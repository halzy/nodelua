-module(nodelua).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_script/2, stop_script/1]).
-export([require/3, send/2, reply/2]).
-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ifdef(TEST).
-export([callback_test_process/1]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	lua :: reference(),
    owner :: pid()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_script(any(), nonempty_string()) -> {ok, pid()}.
start_script(Ref, LuaScript) ->
    supervisor:start_child(nodelua_sup, 
        {{?MODULE, Ref}, {?MODULE, start_link, [LuaScript]}, 
            permanent, 5000, worker, [?MODULE]}).

-spec stop_script(any()) -> ok | {error, not_found}.
stop_script(Ref) ->
    case supervisor:terminate_child(nodelua_sup, {?MODULE, Ref}) of
        ok ->
            supervisor:delete_child(nodelua_sup, {?MODULE, Ref});
        {error, Reason} ->
            {error, Reason}
    end.

-spec start_link(nonempty_string()) -> {ok, pid()}.
start_link(LuaScript) ->
    gen_server:start_link(?MODULE, [{script, LuaScript}, {owner, self()}], []).

-spec send(pid(), term()) -> {ok, reference()} | {error, string()}.
send(Pid, Message) ->
	CallToken = erlang:make_ref(),
	ok = gen_server:cast(Pid, {send, self(), CallToken, Message}),
	{ok, CallToken}.

-spec require(pid(), [binary()], binary()) -> ok.
require(Pid, Path, Module) ->
	gen_server:call(Pid, {require, Path, Module}).

reply(LuaCallback, Response) ->
    Lua = lists:nth(1, LuaCallback),
    CallbackId = lists:nth(2, LuaCallback),
    nlua:send(Lua, [{type, reply}, {pid, self()}, {callback_id, CallbackId}, {reply, Response}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{script, LuaScript}, {owner, Owner}]) ->
	{ok, Script} = file:read_file(LuaScript),
    {ok, LuaReference} = nlua:load(Script, self()),
    {ok, #state{lua=LuaReference, owner=Owner}}.

handle_call({require, Path, Module}, _From, State) ->
	CallToken = erlang:make_ref(),
    nlua:send( State#state.lua, [{type, require}, {pid, self()}, {token, CallToken}, {path, Path}, {module, Module}]),
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
    lager:error("lua_module:handle_call(~p) called!", [Request]),
    {reply, undefined, State}.


handle_cast({send, _From, CallToken, Message}, State) ->
	nlua:send( State#state.lua, [{type, mail}, {pid, self()}, {token, CallToken}, {data, Message}]),
	{noreply, State};
handle_cast(Msg, State) ->
    lager:error("lua_module:handle_cast(~p) called!", [Msg]),
    {noreply, State}.

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
    nodelua_app:start(),
    {ok,Pid} = ?MODULE:start_script(test, "../scripts/main.lua"),
    Pid.
    
cleanup(_Pid) ->
    ?MODULE:stop_script(test).

main_test_() ->
    {foreach,
		fun setup/0,
		fun cleanup/1,
		[
			fun run_require_error/1,
			fun run_callback/1,
            fun run_bogus_call/1,
            fun run_bogus_cast/1
        ]
	}.

run_bogus_call(Pid) ->
    ?_assertEqual(gen_server:call(Pid, bla), undefined).

run_bogus_cast(Pid) ->
    ?_assertEqual(gen_server:cast(Pid, bla), ok).

run_require_error(Pid) ->
	{error, <<Error:5/binary, _Message/binary>>} = require(Pid, [<<"../test_scripts">>], <<"require_error">>),
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
	ok = ?MODULE:require(LuaPid, [<<"../scripts/libs">>,<<"../test_scripts">>,<<"../test_scripts/callback_test">>], <<"callback_test">>),
    EchoPid = spawn(?MODULE, callback_test_process, [self()]),
	{ok, _} = ?MODULE:send(LuaPid, [{echo, EchoPid}]),
    Result = receive
       Data -> Data
    end,
    EchoPid ! die,
    ?_assertEqual( <<"async-test">>, Result).

test_unsupported_test_() ->
    [
        ?_assertEqual(code_change(bla, state, bla), {ok, state})
    ].

-endif.

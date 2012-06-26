-module(lua).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([require/3, send/2, reply/2]).

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
	nodelua :: reference(),
    owner :: pid()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(nonempty_string()) -> {ok, pid()}.
start_link(LuaScript) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [{script, LuaScript}, {owner, self()}], []).

-spec send(pid(), term()) -> {ok, reference()} | {error, string()}.
send(Pid, Message) ->
	CallToken = erlang:make_ref(),
	ok = gen_server:cast(Pid, {send, self(), CallToken, Message}),
	{ok, CallToken}.

-spec require(pid(), [binary()], binary()) -> ok.
require(Pid, Path, Module) ->
	gen_server:call(Pid, {require, Path, Module}).

reply(LuaCallback, Response) ->
    {1.0, Lua} = lists:nth(1, LuaCallback),
    {2.0, CallbackId} = lists:nth(2, LuaCallback),
    nodelua:send(Lua, [{type, reply}, {pid, self()}, {callback_id, CallbackId}, {reply, Response}]).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{script, LuaScript}, {owner, Owner}]) ->
	{ok, Script} = file:read_file(LuaScript),
    {ok, LuaReference} = nodelua:load(Script, self()),
    {ok, #state{nodelua=LuaReference, owner=Owner}}.

handle_call({require, Path, Module}, _From, State) ->
	CallToken = erlang:make_ref(),
    nodelua:send( State#state.nodelua, [{type, require}, {pid, self()}, {token, CallToken}, {path, Path}, {module, Module}]),
    receive
    	[{<<"token">>,CallToken},{<<"error">>,Message}] ->
    		{reply, {error, Message}, State};
    	[{<<"token">>,CallToken}] -> 
    		{reply, ok, State}
    end;
handle_call(stop, _From, State) -> 
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast({send, _From, CallToken, Message}, State) ->
	nodelua:send( State#state.nodelua, [{type, mail}, {pid, self()}, {token, CallToken}, {data, Message}]),
	{noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info([{1.0, <<"invoke">>},{2.0,Data},{3.0,Args}], State) ->
	LuaBin = <<"lua_">>,
	LuaModule = << LuaBin/binary, Data/binary >>,
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
    {ok,Pid} = lua:start_link("../scripts/main.lua"), 
    Pid.
    
cleanup(Pid) ->
    gen_server:call(Pid, stop).

main_test_() ->
    {foreach,
		fun setup/0,
		fun cleanup/1,
		[
			fun run_require_error/1,
			fun run_callback/1		]
	}.

run_require_error(Pid) ->
	{error, Error} = lua:require(Pid, [<<"../test_scripts">>], <<"require_error">>),
    ?_assertEqual( <<"error loading module 'require_error' from file '../test_scripts/require_error.lua':\n\t../test_scripts/require_error.lua:4: '=' expected near '!'">>, Error ).	

callback_test_process(Pid) ->
    receive
        die -> ok;
        Message -> 
            {_, Sender} = lists:keyfind(<<"sender">>, 1, Message),
            lua:reply(Sender, [{pid, Pid}]),
            callback_test_process(Pid)
    end.

run_callback(LuaPid) ->
	ok = lua:require(LuaPid, [<<"../scripts/libs">>,<<"../test_scripts">>,<<"../test_scripts/callback_test">>], <<"callback_test">>),
    EchoPid = spawn(?MODULE, callback_test_process, [self()]),
	{ok, _} = lua:send(LuaPid, [{echo, EchoPid}]),
    Result = receive
       Data -> Data
    end,
    EchoPid ! die,
    ?_assertEqual( <<"async-test">>, Result).

-endif.

-module(lua_socket_server).
-behaviour(gen_server).
-behaviour(lua_module).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([lua_call/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    ports :: dict()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

lua_call(Message) ->
    Command = proplists:get_value(<<"command">>, Message),
    Args = proplists:get_value(<<"args">>, Message),
    gen_server:cast(?SERVER, {Command, Args}).

-spec make_cowboy_id(non_neg_integer()) -> list().
make_cowboy_id(Port) -> 
    lists:flatten(["socket_server_",erlang:integer_to_list(Port)]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    application:start(cowboy),
    {ok, #state{ports=dict:new()}}.

handle_call(stop, _From, State) -> 
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({<<"new">>, Args}, State) ->
    Lua = proplists:get_value(<<"lua">>, Args),
    Port = erlang:round(proplists:get_value(<<"port">>, Args)),

    CowboyID = make_cowboy_id(Port),

    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [{'_', lua_ws_handler, [{lua, Lua}]}]}
    ],

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    {ok, _CowboyPid} = cowboy:start_listener(CowboyID, 128,
        cowboy_tcp_transport, [{port, Port}, {reuseaddr, true}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

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
			fun socket_server/1
		]
	}.

make_ws_request(Host, Port, Path) ->
    "GET "++ Path ++" HTTP/1.1\r\n" ++ 
    "Upgrade: WebSocket\r\nConnection: Upgrade\r\n" ++ 
    "Host: " ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "\r\n" ++
    "Origin: " ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "\r\n" ++
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ++
    "Sec-WebSocket-Version: 13\r\n\r\n".


socket_server(LuaPid) ->
	{ok, ServerPid} = lua_socket_server:start_link(),
	ok = lua:require(LuaPid, [<<"../scripts/libs">>,<<"../test_scripts">>], <<"socket_server_test">>),
	gen_server:call(ServerPid, stop),

    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8080, [binary, {active, false}]),
    Request = make_ws_request("localhost", 8080, "/"),
    ok = gen_tcp:send(Socket, Request),

    <<"on_init">> = receive
        OnInit -> OnInit
    end,

    {ok, <<"HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nSec-Websocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\nConnection: Upgrade\r\n\r\n">>} = gen_tcp:recv(Socket, 0),
    ok = gen_tcp:send(Socket, << 16#82, 16#85, 16#37, 16#fa, 16#21, 16#3d, 16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),

    <<"on_data">> = receive
        OnData -> OnData
    end,

    {ok, << 1:1, 0:3, 2:4, 0:1, 7:7, "goodbye" >>} = gen_tcp:recv(Socket, 0, 6000),
    ok = gen_tcp:close(Socket),

    <<"on_terminate">> = receive
        OnTerminate -> OnTerminate
    end,


    ?_assertEqual(ok, ok).

make_cowboy_id_test_() ->
    [
        ?_assertEqual("socket_server_8080", make_cowboy_id(8080)),
        ?_assertEqual("socket_server_80", make_cowboy_id(80))
    ].

-endif.

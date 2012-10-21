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
-module(nlua_websocket_server).
-behaviour(gen_server).
-behaviour(nlua_module).
-define(SERVER, ?MODULE).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API.
-export([lua_call/2]).

%% API.
-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    ports :: dict()
}).

%% API.

-spec start_link() -> {'ok',pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lua_call(nlua:lua_ref(), [{binary(),any()},...]) -> ok.
lua_call(LuaRef, Message) ->
    Command = proplists:get_value(<<"command">>, Message),
    Args = proplists:get_value(<<"args">>, Message),
    gen_server:cast(?SERVER, {lua_call, Command, Args, LuaRef}).

-spec make_cowboy_id(non_neg_integer()) -> list().
make_cowboy_id(Port) -> 
    lists:flatten(["websocket_server_",erlang:integer_to_list(Port)]).

-spec init([]) -> {ok, #state{}}.
init(_Args) ->
    {ok, #state{ports=dict:new()}}.

handle_call(Request, _From, State) ->
    lager:error("lua_websocket_server:handle_call(~p) called!", [Request]),
    {reply, undefined, State}.

-spec handle_cast({binary(), [{binary(), nlua:lua_ref() | integer()}]}, #state{}) -> {noreply, #state{}}.
handle_cast({lua_call, <<"new">>, Args, LuaRef}, State) ->
    Port = proplists:get_value(<<"port">>, Args),

    CowboyID = make_cowboy_id(Port),

    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [{'_', nlua_ws_handler, [{lua, LuaRef}]}]}
    ],

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    {ok, _CowboyPid} = cowboy:start_http(CowboyID, 128,
        [{port, Port}, {reuseaddr, true}],
        [{dispatch, Dispatch}]
    ),

    % FIXME: update the state with used ports
    {noreply, State};
handle_cast({lua_call, <<"delete">>, Args, _LuaRef}, State) ->
    Port = proplists:get_value(<<"port">>, Args),

    CowboyID = make_cowboy_id(Port),
    cowboy:stop_listener(CowboyID),
    
    % FIXME: update the state with used ports
    {noreply, State};
handle_cast(Msg, State) ->
    lager:error("lua_websocket_server:handle_cast(~p) called!", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:error("lua_websocket_server:handle_info(~p) called!", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-ifdef(TEST).

make_ws_request(Host, Port, Path) ->
    "GET "++ Path ++" HTTP/1.1\r\n" ++ 
    "Upgrade: WebSocket\r\nConnection: Upgrade\r\n" ++ 
    "Host: " ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "\r\n" ++
    "Origin: " ++ Host ++ ":" ++ erlang:integer_to_list(Port) ++ "\r\n" ++
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ++
    "Sec-WebSocket-Version: 13\r\n\r\n".

websocket_server_test() ->
    nodelua:start(), 

    {ok, LuaPid} = nodelua:start_script(test_websocket_server, "../scripts/main.lua", [{path, [<<"../scripts/libs">>,<<"../test_scripts">>]}, {module, <<"websocket_server_test">>}]),

    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8080, [binary, {active, false}]),
    Request = make_ws_request("localhost", 8080, "/"),
    ok = gen_tcp:send(Socket, Request),

    {lua_message, LuaPid, <<"on_init">>} = receive
        OnInit -> OnInit
    end,

    {ok, <<"HTTP/1.1 101 Switching Protocols\r\nconnection: Upgrade\r\nUpgrade: websocket\r\nSec-Websocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n\r\n">>} = gen_tcp:recv(Socket, 0),

    % binary
    ok = gen_tcp:send(Socket, << 16#82, 16#85, 16#37, 16#fa, 16#21, 16#3d, 16#7f, 16#9f, 16#4d, 16#51, 16#59 >>),
    {lua_message, LuaPid, <<"on_data">>} = receive
        OnData1 -> OnData1
    end,
    % binary-response
    {ok, << 1:1, 0:3, 2:4, 0:1, 7:7, "goodbye" >>} = gen_tcp:recv(Socket, 0, 6000),

    % text
    ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d, 16#7f, 16#9f, 16#4d, 16#51, 16#60 >>),
    {lua_message, LuaPid, <<"on_data">>} = receive
        OnData2 -> OnData2
    end,
    % binary-response
    {ok, << 1:1, 0:3, 1:4, 0:1, 7:7, "goodbye" >>} = gen_tcp:recv(Socket, 0, 6000),

    ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 0:8 >>), %% ping
    {ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong

    ok = gen_tcp:send(Socket, << 1:1, 0:3, 10:4, 0:8 >>), %% pong

    ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 0:8 >>), %% close
    {ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
    {error, closed} = gen_tcp:recv(Socket, 0, 6000),

    {lua_message, LuaPid, <<"on_terminate">>} = receive
        OnTerminate -> OnTerminate
    end,

    % @@@ FIXME: stop message not supported
    % have the cowboy server shut down somehow,
    % perhaps tie it to the life of the lua script
    % ? make the shutdown main.lua call shutdown on
    % the websocket stuff too
    %gen_server:call(ServerPid, stop),

    nodelua:stop_script(test_websocket_server),
    nodelua:stop(),

    ?assertEqual(ok, ok).

make_cowboy_id_test_() ->
    [
        ?_assertEqual("websocket_server_8080", make_cowboy_id(8080)),
        ?_assertEqual("websocket_server_80", make_cowboy_id(80))
    ].

%% call functions that we do not use
% test_unsupported_test_() ->
%     [
%         ?_assertEqual(gen_server:call(nlua_websocket_server, bla), undefined),
%         ?_assertEqual(gen_server:cast(nlua_websocket_server, bla), ok),
%         ?_assertEqual(code_change(bla, state, bla), {ok, state})
%     ].

-endif.

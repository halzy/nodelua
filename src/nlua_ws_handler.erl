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

-module(nlua_ws_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-record(state, {
    lua :: any()
}).

send_lua(Event, Data, Req1, State) ->
	{Port, Req2} = cowboy_req:port(Req1),
	Message = [{type, type_websocket_server}, {socket, self()}, {port, Port}, {event, Event}, {data, Data}],
	nlua:send(State#state.lua, Message),
	Req2.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req1, Opts) ->
	Lua = proplists:get_value(lua, Opts),
	State = #state{lua=Lua},
	Req2 = send_lua(init, <<"">>, Req1, State),
    {ok, Req2, #state{lua=Lua}}.

websocket_handle({text, Data}, Req, State) ->
	ReqSend = send_lua(data, Data, Req, State),
	{ok, ReqSend, State};
websocket_handle({binary, Data}, Req, State) ->
	ReqSend = send_lua(data, Data, Req, State),
	{ok, ReqSend, State};
websocket_handle({ping, _Data}, Req, State) ->
	{ok, Req, State};
websocket_handle({pong, _Data}, Req, State) ->
	{ok, Req, State}.

websocket_info([{<<"bin">>, Msg}], Req, State) ->
	{reply, {binary, Msg}, Req, State};
websocket_info([{<<"text">>, Msg}], Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(Msg, Req, State) ->
	lager:error("lua_ws_handler:websocket_info(~p) called!", [Msg]),
    {ok, Req, State}.

websocket_terminate(_Reason, Req, State) ->
	send_lua(terminate, <<"">>, Req, State),
    ok.

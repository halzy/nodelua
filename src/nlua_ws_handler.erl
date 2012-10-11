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
-module(nlua_ws_handler).
-behaviour(cowboy_websocket_handler).

%% API.
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {
    lua :: nlua:lua_ref(),
    port :: inet:port_number()
}).

send_lua(Event, Data, Req, State) ->
	Message = [{type, type_websocket_server}, {socket, self()}, {port, State#state.port}, {event, Event}, {data, Data}],
	case nlua:send(State#state.lua, Message) of
        ok -> 
            {ok, Req, State, hibernate};
        {error, ErrorMessage} -> 
            lager:error("Error sending to lua: ~p", [ErrorMessage]),
            {shutdown, Req}
    end.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Opts) ->
	Lua = proplists:get_value(lua, Opts),
    {Port, Req1} = cowboy_req:port(Req),
	State = #state{lua=Lua,port=Port},
	send_lua(init, <<"">>, cowboy_req:compact(Req1), State).

websocket_handle({text, Data}, Req, State) ->
	send_lua(data, Data, Req, State);
websocket_handle({binary, Data}, Req, State) ->
	send_lua(data, Data, Req, State);
websocket_handle({ping, _Data}, Req, State) ->
	{ok, Req, State, hibernate};
websocket_handle({pong, _Data}, Req, State) ->
	{ok, Req, State, hibernate}.

%% API for lua -- lua sends us messages
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

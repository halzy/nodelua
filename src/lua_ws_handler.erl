-module(lua_ws_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-record(state, {
    lua :: any()
}).

send_lua(Event, Data, Req1, State) ->
	{Port, Req2} = cowboy_http_req:port(Req1),
	Message = [{type, type_websocket_server}, {socket, self()}, {port, Port}, {event, Event}, {data, Data}],
	nodelua:send(State#state.lua, Message),
	Req2.

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_websocket}.

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

-module(lua_ws_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3]).

-record(state, {
    lua :: any()
}).

send_lua(Event, Data, Req1, State) ->
	{Port, Req2} = cowboy_http_req:port(Req1),
	Message = [{type, type_socket_server}, {socket, self()}, {port, Port}, {event, Event}, {data, Data}],
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
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(Msg, Req, State) when is_binary(Msg) ->
	{reply, {binary, Msg}, Req, State};
websocket_info(Msg, Req, State) when is_list(Msg) ->
    {reply, {text, Msg}, Req, State};
websocket_info(Msg, Req, State) ->
	erlang:display("websocket_info():"),
	erlang:display(Msg),
    {ok, Req, State}.

websocket_terminate(_Reason, Req, State) ->
	send_lua(terminate, <<"">>, Req, State),
    ok.

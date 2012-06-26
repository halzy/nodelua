
require "mailbox"

local M = {}
local module_name = "socket_server"

M.on_init_callbacks = {}
M.on_data_callbacks = {}
M.on_terminate_callbacks = {}

local function delete(port)
	M.on_init_callbacks[port] = nil
	M.on_data_callbacks[port] = nil
	M.on_terminate_callbacks[port] = nil
	mailbox.send(mailbox.parent(), {"invoke",module_name,{command="delete",port=8080}})
end; M.delete = delete;

local function new(port, on_init, on_data, on_terminate)
	if on_init == nil or on_data == nil or on_terminate == nil then
		error("socket_server.new does not accept nil callbacks")
	end
	if M.on_data_callbacks[port] then
		delete(port)
	end

	M.on_init_callbacks[port] = on_init
	M.on_data_callbacks[port] = on_data
	M.on_terminate_callbacks[port] = on_terminate

	mailbox.send(mailbox.parent(), {"invoke",module_name,{command="new",args={lua=mailbox.self(), port=8080}}})
end; M.new = new;

local function send(socket, message)
	mailbox.send(socket, message)
end; M.send = send;

local function update(message)
	local event = message.event
	local port = message.port
	local socket = message.socket
	if event == "data" then
		local on_data = M.on_data_callbacks[port]

		if on_data then
			on_data(socket, message.data)
		end
	elseif event == "init" then
		local on_init = M.on_init_callbacks[port]
		if on_init then
			on_init(socket)
		end
	elseif event == "terminate" then
		local on_terminate = M.on_terminate_callbacks[port]
		if on_terminate then
			on_terminate(socket)
		end
	end
end

mailbox.register_type(module_name, update)

return M

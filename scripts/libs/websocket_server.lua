--[[
Copyright (c) 2012 Benjamin Halsted <bhalsted@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the"Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
--]]

require "mailbox"

local M = {}
local module_name = "websocket_server"

M.on_init_callbacks = {}
M.on_data_callbacks = {}
M.on_terminate_callbacks = {}

local function delete(port)
	M.on_init_callbacks[port] = nil
	M.on_data_callbacks[port] = nil
	M.on_terminate_callbacks[port] = nil
	mailbox.send(mailbox.parent(), {"invoke",module_name,{command="delete",args={port=port}}})
end; M.delete = delete;

local function new(port, on_init, on_data, on_terminate)
	if on_init == nil or on_data == nil or on_terminate == nil then
		error("websocket_server.new does not accept nil callbacks")
	end
	if M.on_data_callbacks[port] then
		delete(port)
	end

	M.on_init_callbacks[port] = on_init
	M.on_data_callbacks[port] = on_data
	M.on_terminate_callbacks[port] = on_terminate

	mailbox.send(mailbox.parent(), {"invoke",module_name,{command="new",args={port=port}}})
end; M.new = new;

local function send_text(socket, message)
	mailbox.send(socket, {text=message})
end; M.send_text = send_text;

local function send_binary(socket, message)
	mailbox.send(socket, {bin=message})
end; M.send_binary = send_binary;

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

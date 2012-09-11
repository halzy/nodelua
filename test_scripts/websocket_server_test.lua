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

local websocket_server = require("websocket_server")

local M = {}

local function dump(o)
	if type(o) == 'table' then
		local s = '{ '
		for k,v in pairs(o) do
			if type(k) ~= 'number' then k = '"'..k..'"' end
			s = s .. '['..k..'] = ' .. dump(v) .. ','
		end
		return s .. '} '
	else
		return tostring(o)
	end
end

local function update(inbox)
	print("websocket_server_test:update(): " .. dump(inbox));
	for k,v in pairs(inbox) do
	end
end; M.update = update

local function shutdown()
	print("socket test shutting down")
end; M.shutdown = shutdown

local function on_init( socket )
	mailbox.send(mailbox.parent(), "on_init")
	mailbox.send(socket, "testing bogus message sending")
end
local function on_data( socket, data )
	if "Helln" == data then
		websocket_server.send_binary( socket, "goodbye")
	elseif "HellW" == data then
		websocket_server.send_text( socket, "goodbye")
	end
	mailbox.send(mailbox.parent(), "on_data")
end
local function on_terminate( socket )
	mailbox.send(mailbox.parent(), "on_terminate")
end

local server = websocket_server.new(8080, on_init, on_data, on_terminate);

return M

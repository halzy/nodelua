

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

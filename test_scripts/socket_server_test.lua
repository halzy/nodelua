

local socket_server = require("socket_server")

local server = socket_server.new();

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
	print("socket_server_test:update(): " .. dump(inbox));
	for k,v in pairs(inbox) do
	end
end; M.update = update

local function shutdown()
	printf("socket test shutting down")
end; M.shutdown = shutdown

return M


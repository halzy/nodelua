
local module_test = require("module_test")

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

local function callback(result) 
	mailbox.send(result.pid, "async-test")
end

local function update(inbox)
	for k,v in pairs(inbox) do
		if v.echo then
			mailbox.sendasync(v.echo, "sending async", callback)
		end
	end
end; M.update = update

return M

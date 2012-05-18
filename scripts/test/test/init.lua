
local something = require("something")

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
	print("callback" .. dump(result)) 
end

local function update(inbox)
	for k,v in pairs(inbox) do
		if v.echo then
			mailbox.sendasync(v.echo, "echo test", callback)
		end
	end
end; M.update = update

return M

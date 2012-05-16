
local something = require("something")

local M = {}

local function update(inbox)
	for k,v in pairs(inbox) do
		something.print(k,v)
	end
end; M.update = update

return M

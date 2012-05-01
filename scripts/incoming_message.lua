require "mailbox"

function dump(o)
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

function main()
	while true do
		print("incoming")
		while true do
			local message = mailbox.next()
			if message == nil then break end
			print(dump(message))
			if 'table' == type(message) then
				local pid = message["pid"]
				local msg = message.message
				if pid then
					mailbox.send(pid, msg)
				end
			end
		end
		coroutine.yield()
	end
end

main()

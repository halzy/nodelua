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
		message = mailbox.next()
		while message do
			print(dump(message));
			message = mailbox.next() 
		end
		coroutine.yield()
	end
end

main()

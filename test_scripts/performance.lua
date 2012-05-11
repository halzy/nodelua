
require "mailbox"

function main()
	while true do
		while true do
			local message = mailbox.next()
			if message == nil then break end
			mailbox.send(message.pid, message.data)
		end
		coroutine.yield()
	end
end

main()

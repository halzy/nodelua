
require "mailbox"

function main()
	while true do
		while true do
			local message = mailbox.next()
			if message == nil then break end
			mailbox.send(mailbox.parent(), mailbox.parent())
		end
		coroutine.yield()
	end
end

main()

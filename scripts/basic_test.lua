require "mailbox"

function main()
	while true do
		mailbox.next()
		coroutine.yield()
	end
end

main()

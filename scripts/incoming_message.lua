require "mailbox"

function main()
	print("incoming")
	while true do
		print(mailbox.next());
		coroutine.yield()
	end
end

main()

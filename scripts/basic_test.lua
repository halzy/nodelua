function main()
	print('hello world')
	while true do
		coroutine.yield()
	end
end

main()

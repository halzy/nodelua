
require "mailbox"

mailbox.async_id = 0
mailbox.async_callbacks = {}
local function sendasync(pid, msg, callback)
	local callback_id = mailbox.async_id + 1
	local address = mailbox.address()
	mailbox.async_id = callback_id
	mailbox.async_callbacks[callback_id] = callback;
	mailbox.send(pid, {sender={address, callback_id}, data={message=msg}} )
end
mailbox.sendasync = sendasync

function main()
	local inbox = {}
	local update_function = nil
	local shutdown_function = nil;

	local mail_sorter = {}
	function mail_sorter.mail(message) 
		table.insert(inbox, message.data) 
	end
	function mail_sorter.load(message) 
		local new_path = {}
		for index, module_path in ipairs(message.path) do
			table.insert(new_path, module_path .. "/?.lua;" .. module_path .. "/?/init.lua;" .. module_path .. "/?/?.lua")
		end

		package.cpath = "" -- do not allow for C module loading
		package.path = table.concat( new_path, ";" )

		local success, behavior = pcall(require, message.module)
		if( success ) then
			if( "function" == type(behavior.update) ) then
				update_function = behavior.update
			end
			if( "function" == type(behavior.shutdown) ) then
				shutdown_function = behavior.shutdown
			end
		end

		local response = {token=message.token}
		if( not success ) then
			response["error"] = behavior
		end

		mailbox.send(message.pid, response)
	end
	function mail_sorter.reply(message)
		local callback_id = message.callback_id
		local reply = message.reply
		local callback = mailbox.async_callbacks[callback_id];
		mailbox.async_callbacks[callback_id] = nil;
		callback(reply);
	end

	while true do
		inbox = {}
		while true do
			local message = mailbox.next()
			if message == nil then break end

			-- check for certain types of message (load, kill, etc)
			-- save the message into a table to give the script
			mail_sorter[message.type](message)
		end

		-- run the script if we have one, giving it the messages
		if( update_function ) then
			assert(pcall(update_function, inbox))
		end

		-- somehow let the script send messages

		if( mailbox.shutting_down() ) then
			if( shutdown_function ) then 
				shutdown_function()
			end
			return;
		end

		coroutine.yield()
	end
end

main()

require "mailbox"

mailbox.async_id = 0
mailbox.async_callbacks = {}
local function sendasync(pid, msg, callback)
	local callback_id = mailbox.async_id + 1
	mailbox.async_id = callback_id
	mailbox.async_callbacks[callback_id] = callback;
	mailbox.send(pid, {callback=callback_id, message=msg} )
end
mailbox.sendasync = sendasync

function main()
	local inbox = {}
	local update_function = nil
	local destroy_function = nil;

	local mail_sorter = {}
	function mail_sorter.mail(message) 
		table.insert(inbox, message.data) 
	end
	function mail_sorter.load(message) 
		package.path = message.path .. "/" .. message.module .. "/?.lua;" .. message.path .. "/" .. message.module .. "/?/init.lua"

		local behavior = assert(require(message.module))
		if( behavior ) then
			if( "function" == type(behavior.update) ) then
				update_function = behavior.update
			end
			if( "function" == type(behavior.destroy) ) then
				destroy_function = behavior.destroy
			end
		end
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
			local upfunc = mail_sorter[message.type]
			upfunc(message)
		end

		-- run the script if we have one, giving it the messages
		if( update_function ) then
			assert(pcall(update_function, inbox))
		end

		-- somehow let the script send messages

		coroutine.yield()
	end
end

main()

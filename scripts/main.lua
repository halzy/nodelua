--[[
Copyright (c) 2012 Benjamin Halsted <bhalsted@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the"Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
--]]

require "mailbox"

mailbox.async_id = 0
mailbox.async_callbacks = {}
local function sendasync(pid, msg, callback)
	local callback_id = mailbox.async_id + 1
	local self = mailbox.self()
	mailbox.async_id = callback_id
	mailbox.async_callbacks[callback_id] = callback;
	mailbox.send(pid, {sender={self, callback_id}, data={message=msg}} )
end
mailbox.sendasync = sendasync

function main()
	local update_function = nil
	local shutdown_function = nil;

	local mail_sorter = {}
	function mail_sorter.mail(message) 
		-- run the script if we have one, giving it the messages
		if( update_function ) then
			assert(pcall(update_function, message.data))
		end
	end
	function mail_sorter.require(message) 
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

		local response = {message.token}
		if( not success ) then
			table.insert(response, {error=behavior})
		end

		mailbox.send(message.pid, response)
	end
	function mail_sorter.reply(message)
		local callback_id = message.callback_id
		local reply = message.reply
		local callback = mailbox.async_callbacks[callback_id];
		mailbox.async_callbacks[callback_id] = nil;
		assert(pcall(callback, reply))
	end

	local function register_type(name, callback)
		local message_type = "type_" .. name
		if mail_sorter[message_type] then
			return false
		end
		mail_sorter[message_type] = callback
		return true
	end
	mailbox.register_type = register_type

	for status, message in mailbox.iterator() do
		-- check for certain types of message (load, kill, etc)
		-- save the message into a table to give the script
		mail_sorter[message.type](message)
	end

	-- somehow let the script send messages
	if( shutdown_function ) then 
		assert(pcall(shutdown_function))
	end

end

main()

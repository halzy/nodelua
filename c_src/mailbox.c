#include "mailbox.h"
#include "message_queue.h"
#include "terminator.h"

//#include <lualib.h>
#include <lauxlib.h>

#define MESSAGES "messages"

static message_queue_ptr get_messages(lua_State* lua)
{
	// stack: -
	lua_getfield(lua, LUA_ENVIRONINDEX, MESSAGES);
	// stack: userdata(messages)
	message_queue_ptr messages = (message_queue_ptr) lua_touserdata(lua, -1);
	// stack: -
	lua_pop(lua, 1);

	return messages;
}

static int next_message(lua_State* lua)
{
	// stack: -
	message_queue_ptr messages = get_messages(lua);

	// stack: -
	ERL_NIF_TERM message;
	int had_message  = message_queue_pop( messages, &message);
	if(had_message)
	{
		ErlNifEnv* env = message_queue_process_getenv( messages );
		terminator_tolua(lua, message, env);
	}
	else
	{
		lua_pushnil(lua);
	}

	// stack: nil || message

	return 1;
}

static int send_message(lua_State* lua)
{
	// stack: - pid, message
	message_queue_ptr messages = get_messages(lua);

	// who do we send the message to?
	ErlNifEnv* env = message_queue_process_getenv( messages );

	// validate some args
	terminator_lua_checkpid(lua, 1);
	luaL_checkany(lua, 2);

	ERL_NIF_TERM message;
	if(terminator_toerl(lua, &message, env))
	{
		ERL_NIF_TERM pid_term;
		if(terminator_toerl(lua, &pid_term, env))
		{
			ErlNifPid pid;
			if(enif_get_local_pid(env, pid_term, &pid))
			{
				if(enif_send(env, &pid, env, message))
				{
					printf("sent message\n");
				}
			}
		}
	}


	return 0;
}

static const struct luaL_Reg mailbox [] = {
	{"next", next_message},
	{"send", send_message},
	{NULL, NULL}
};

LUALIB_API int luaopen_mailbox(lua_State *lua)
{
	// stack: userdata(mailbox)
	terminator_create_types(lua);

	// stack: userdata(mailbox)
	lua_newtable (lua);
	lua_replace (lua, LUA_ENVIRONINDEX);

	// stack: userdata(mailbox)
	luaL_register(lua, "mailbox", mailbox);

	// stack: userdata(mailbox), table(mailbox)
	lua_insert(lua, 1);

	// stack: table(mailbox), userdata(mailbox)
	lua_setfield (lua, LUA_ENVIRONINDEX, MESSAGES);

	// stack: table(mailbox)
	return 1;
}

#include "mailbox.h"
#include "message_queue.h"
#include "terminator.h"

//#include <lualib.h>
#include <lauxlib.h>

#define MESSAGES "messages"

static message_queue_ptr get_messages(lua_State* lua)
{
	// stack: -
	lua_pushvalue(lua, lua_upvalueindex(1));

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
	ErlNifEnv* env = message_queue_sending_getenv( messages );

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
				enif_send(NULL, &pid, env, message);
			}
		}
	}
	enif_clear_env(env);

	return 0;
}

static const struct luaL_Reg mailbox_funcs [] = {
	{"next", next_message},
	{"send", send_message},
	{NULL, NULL}
};

LUALIB_API int luaopen_mailbox(lua_State *lua)
{
	get_messages(lua);
	terminator_create_types(lua);

	luaL_newlibtable(lua, mailbox_funcs);

	lua_pushvalue(lua, lua_upvalueindex(1));

	// setfuncs will take the items between the table and the 
	// top and make them upvalues for all functions (lua 5.2)
	luaL_setfuncs(lua, mailbox_funcs, 1);

	return 1;
}



LUALIB_API int do_open_mailbox(lua_State *lua)
{
	lua_pushvalue(lua, lua_upvalueindex(1));
	lua_pushcclosure(lua, luaopen_mailbox, 1);

	lua_pushliteral(lua, "mailbox"); /* argument to open function */
	lua_call(lua, 1, 1);  /* open module */

	// put the module into the loaded table
	luaL_getsubtable(lua, LUA_REGISTRYINDEX, "_LOADED");
	lua_pushvalue(lua, -2);  // make copy of module (call result)
	lua_setfield(lua, -2, "mailbox");	// _LOADED[modname] = module
	lua_pop(lua, 1);  					// remove _LOADED table

	// put the module into the global table
    lua_pushglobaltable(lua);
    lua_pushvalue(lua, -2);  			// copy of 'module'
    lua_setfield(lua, -2, "mailbox");	// _G[modname] = module
    lua_pop(lua, 1);  					// remove _G table

	return 1;
}



void register_mailbox(lua_State *lua, message_queue_ptr messages)
{
	int top = lua_gettop(lua);
	lua_getfield(lua, LUA_REGISTRYINDEX, "_PRELOAD");
	int registry = lua_gettop(lua);

	lua_pushliteral(lua, "mailbox");
	lua_pushlightuserdata(lua, messages);
	lua_pushcclosure(lua, do_open_mailbox, 1);
	lua_settable(lua, registry);

	lua_settop(lua, top);
}

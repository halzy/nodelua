#include "mailbox.h"
#include "message_queue.h"
#include "terminator.h"

//#include <lualib.h>
#include <lauxlib.h>

#define MESSAGES "messages"

static int next_message(lua_State* lua)
{
	lua_getfield(lua, LUA_ENVIRONINDEX, MESSAGES);
	message_queue_ptr messages = (message_queue_ptr) lua_touserdata(lua, -1);
	lua_pop(lua, 1);

	ErlNifEnv* env;
	ERL_NIF_TERM message;
	int had_message  = message_queue_pop( messages, &message, &env);
	if(had_message)
	{
		terminator_tolua(lua, message, env);
	}
	else
	{
		lua_pushnil(lua);
	}
	return 1;
}

static const struct luaL_Reg mailbox [] = {
	{"next", next_message},
	{NULL, NULL}
};

LUALIB_API int luaopen_mailbox(lua_State *lua)
{
	// stack: userdata(mailbox)
	lua_newtable (lua);
	lua_replace (lua, LUA_ENVIRONINDEX);
	luaL_register(lua, "mailbox", mailbox);

	// stack: userdata(mailbox), table(mailbox)
	lua_insert(lua, 1);

	// stack: table(mailbox), userdata(mailbox)
	lua_setfield (lua, LUA_ENVIRONINDEX, MESSAGES);

	// stack: table(mailbox)
	return 1;
}

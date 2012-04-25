#include "terminator.h"

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	if(enif_is_number(env, message))
	{
		double value;
		enif_get_double(env, message, &value);
		lua_pushnumber(lua, value);
	}
	else
	{
		lua_pushnil(lua);
	}
}

#ifndef NODELUA_TERMINATOR
#define NODELUA_TERMINATOR TRUE

#include <lua.h>
#include <erl_nif.h>

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env);
void terminator_create_types(lua_State* lua);

#endif
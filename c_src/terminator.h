#ifndef NODELUA_TERMINATOR
#define NODELUA_TERMINATOR TRUE

#include <lua.h>
#include <erl_nif.h>

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env);

#endif
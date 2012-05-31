#ifndef NODELUA_TERMINATOR
#define NODELUA_TERMINATOR TRUE

#include <lua.h>
#include <erl_nif.h>

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env, ErlNifResourceType* resource_type);
int  terminator_toerl(lua_State* lua, ERL_NIF_TERM *result, ErlNifEnv* env, ErlNifResourceType* resource_type);

void terminator_create_types(lua_State* lua, void* state_work);
void* terminator_lua_checkpid(lua_State* lua, int index);

void terminator_tolua_luaaddress(lua_State* lua, void* reference);
void terminator_tolua_erlpid(lua_State* lua, ErlNifPid erlpid);

#endif
/**
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
**/

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
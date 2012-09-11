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

#include "mailbox.h"
#include "message_queue.h"
#include "terminator.h"
#include "state.h"

#include <assert.h>

//#include <lualib.h>
#include <lauxlib.h>

#define MESSAGES "messages"

static message_queue_ptr get_messages(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);

	// stack: -
	lua_pushvalue(lua, lua_upvalueindex(1));

	// stack: userdata(messages)
	message_queue_ptr messages = (message_queue_ptr) lua_touserdata(lua, -1);
	// stack: -
	lua_pop(lua, 1);

	assert(NULL != messages);
	assert(lua_gettop(lua) == top);

	return messages;
}

static ErlNifResourceType* get_resource_type(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);

	// stack: -
	lua_pushvalue(lua, lua_upvalueindex(3));

	// stack: userdata(messages)
	ErlNifResourceType* resource_type = (ErlNifResourceType*) lua_touserdata(lua, -1);
	// stack: -
	lua_pop(lua, 1);

	assert(NULL != resource_type);

	assert(lua_gettop(lua) == top);
	return resource_type;
}


static erllua_ptr* get_erllua(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);

	// stack: -
	lua_pushvalue(lua, lua_upvalueindex(2));

	// stack: userdata(messages)
	erllua_ptr* erllua = (erllua_ptr*) lua_touserdata(lua, -1);
	// stack: -

	lua_pop(lua, 1);

	assert(NULL != erllua);
	assert(lua_gettop(lua) == top);
	return erllua;
}



static void* get_state_work(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);

	// stack: -
	lua_pushvalue(lua, lua_upvalueindex(4));

	// stack: userdata(messages)
	void* state_work = (void*) lua_touserdata(lua, -1);
	// stack: -

	lua_pop(lua, 1);

	assert(NULL != state_work);

	assert(lua_gettop(lua) == top);
	return state_work;
}



static int next_message(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);

	// stack: -
	message_queue_ptr messages = get_messages(lua);
	ErlNifResourceType* resource_type = get_resource_type(lua);

	// stack: -
	ERL_NIF_TERM message;
	int had_message  = message_queue_pop( messages, &message);
	if(had_message)
	{
		ErlNifEnv* env = message_queue_process_getenv( messages );
		terminator_tolua(lua, message, env, resource_type);
	}
	else
	{
		lua_pushnil(lua);
	}

	// stack: nil || message
	assert(lua_gettop(lua) == top+1);
	return 1;
}

static int send_message(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);
	
	// stack: - pid, message
	message_queue_ptr messages = get_messages( lua );

	// who do we send the message to?
	ErlNifEnv* env = message_queue_sending_getenv( messages );
	// validate some args
	terminator_lua_checkpid(lua, 1);
	luaL_checkany(lua, 2);

	ErlNifResourceType* resource_type = get_resource_type(lua);

	ERL_NIF_TERM message;
	if(terminator_toerl(lua, &message, env, resource_type))
	{
		ERL_NIF_TERM pid_term;
		if(terminator_toerl(lua, &pid_term, env, resource_type))
		{
			ErlNifPid pid;
			if(enif_get_local_pid(env, pid_term, &pid))
			{
				enif_send(NULL, &pid, env, message);
			}
		}
	}
	enif_clear_env(env);

	assert(lua_gettop(lua) == top - 2);

	return 0;
}

static int shutting_down(lua_State* lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);

	// stack: - pid, message
	erllua_ptr erllua = (erllua_ptr) get_erllua( lua );
	lua_pushboolean( lua, erllua_shutting_down(erllua));

	assert(lua_gettop(lua) == top+1);
	return 1;
}

static int get_self(lua_State* lua)
{
	assert(NULL != lua);

	int top = lua_gettop( lua );

	erllua_ptr erllua = (erllua_ptr) get_erllua( lua );
	int shutting_down = erllua_shutting_down(erllua);
	if(shutting_down)
	{
		lua_pushnil( lua );
	}
	else
	{
		terminator_tolua_luaaddress( lua, get_state_work( lua ));
	}

	assert(lua_gettop(lua) == top+1);
	return 1;
}


static int get_parent(lua_State* lua)
{
	assert(NULL != lua);

	int top = lua_gettop( lua );

	erllua_ptr erllua = (erllua_ptr) get_erllua( lua );
	ErlNifPid parent_pid = erllua_parent_pid(erllua);
	terminator_tolua_erlpid( lua, parent_pid );

	assert(lua_gettop(lua) == top+1);
	return 1;
}


static const struct luaL_Reg mailbox_funcs [] = {
	{"next", next_message},
	{"send", send_message},
	{"self", get_self},
	{"parent", get_parent},
	{"shutting_down", shutting_down},
	{NULL, NULL}
};

LUALIB_API int luaopen_mailbox(lua_State *lua)
{
	int top = lua_gettop(lua);
	
	assert(NULL != lua);
	
	void* state_work = get_state_work( lua );
	assert(NULL != state_work);

	terminator_create_types(lua, state_work);

	luaL_newlibtable(lua, mailbox_funcs);

	lua_pushvalue(lua, lua_upvalueindex(1)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushvalue(lua, lua_upvalueindex(2)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushvalue(lua, lua_upvalueindex(3)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushvalue(lua, lua_upvalueindex(4)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox

	// setfuncs will take the items between the table and the 
	// top and make them upvalues for all functions (lua 5.2)
	luaL_setfuncs(lua, mailbox_funcs, 4); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	
	assert(lua_gettop(lua) == top+1);
	return 1;
}



LUALIB_API int do_open_mailbox(lua_State *lua)
{
	assert(NULL != lua);
	int top = lua_gettop(lua);
	
	lua_pushvalue(lua, lua_upvalueindex(1)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushvalue(lua, lua_upvalueindex(2)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushvalue(lua, lua_upvalueindex(3)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushvalue(lua, lua_upvalueindex(4)); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushcclosure(lua, luaopen_mailbox, 4); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox

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

	assert(lua_gettop(lua) == top+1);
	return 1;
}



void register_mailbox(lua_State *lua, erllua_ptr erllua, message_queue_ptr message_queue, ErlNifResourceType* erl_resource_type, void* state_work)
{
	assert(NULL != lua);
	assert(NULL != erllua);
	assert(NULL != message_queue);
	assert(NULL != erl_resource_type);
	assert(NULL != state_work);
	
	int top = lua_gettop(lua);
	lua_getfield(lua, LUA_REGISTRYINDEX, "_PRELOAD");
	int registry = lua_gettop(lua);

	lua_pushliteral(lua, "mailbox");
	lua_pushlightuserdata(lua, message_queue); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushlightuserdata(lua, erllua); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushlightuserdata(lua, erl_resource_type); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushlightuserdata(lua, state_work); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_pushcclosure(lua, do_open_mailbox, 4); // matches do_open_mailbox, register_mailbox, and luaopen_mailbox
	lua_settable(lua, registry);

	lua_settop(lua, top);
}

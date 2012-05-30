#include "node_memory.h"

#include "terminator.h"
#include "nl_util.h"
#include "erllua.h"
#include "state.h"

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <string.h>
#include <assert.h>

#define TYPE_ERL_PID "mailbox.id"
#define TYPE_ERL_REF "nodelua.terminator.ref"
#define TYPE_LUA_ADDRESS "mailbox.address"

typedef struct mailbox_address* mailbox_address_ptr;
struct mailbox_address
{
	state_work_ptr state_work;
	RESOURCE_REF_TYPE ref_type;
};

#define ERROR_STACK_MESSAGE "Could not grow stack large enough to read message."

static void push_nif_term(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env, ErlNifResourceType* resource_type);

// checks to see if the tuple is something we would put as a KV pair in a lua table
// puts the key(0) and value(1) in the array
static int push_table_iskvp(ErlNifEnv* env, ERL_NIF_TERM tuple, const ERL_NIF_TERM** array)
{
	int result = 0;
	int tuple_arity = 0;
	const ERL_NIF_TERM* tuple_terms;
	if(enif_get_tuple(env, tuple, &tuple_arity, &tuple_terms))
	{
		if(tuple_arity == 2)
		{
			result = 1;
			if(NULL != array)
			{
				*array = tuple_terms;
			}
		}
	} 
	else if(NULL != array)
	{
		*array = NULL;
	}

	return result;
}


// pushes the items in the array into the table on the top of the stack
static void push_table_append(lua_State* lua, ErlNifEnv* env, ErlNifResourceType* resource_type, ERL_NIF_TERM array_items[], int array_count)
{
	const int top = lua_gettop(lua);

	int lua_index = 0;
	int index = 0;
	for(index = 0; index < array_count; ++index)
	{
		int index_open = 0;
		do
		{
			++lua_index;
			lua_rawgeti(lua, -1, lua_index);
			index_open = lua_isnil(lua, -1);
			lua_pop(lua, 1);
		} while(!index_open);

		push_nif_term(lua, array_items[index], env, resource_type);
		lua_rawseti(lua, -2, lua_index);
	}

	assert(lua_gettop(lua) == top);
}

// it is expected that a table is on top of the lua stack
static int push_table_set(lua_State* lua, ErlNifEnv* env, ERL_NIF_TERM tuple, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);

	int result = 0;
	const ERL_NIF_TERM* tuple_terms;

	if(push_table_iskvp(env, tuple, &tuple_terms))
	{
		// stack: table
		push_nif_term(lua, tuple_terms[0], env, resource_type);
		// stack: table, key

		int insert = 0;
		// check to see if the term is primitive enough and 
		// if it already exists as a KvP then retun failure
		// so that it can get appended to the list
		int type = lua_type(lua, -1);
		if(type == LUA_TNUMBER || type == LUA_TBOOLEAN || type == LUA_TSTRING 
			|| type == LUA_TUSERDATA || type == LUA_TLIGHTUSERDATA)
		{
			lua_pushvalue (lua, -1);
			// stack: table, key, key

			lua_rawget(lua, -3);
			// stack: table, key, value

			insert = (1 == lua_isnil(lua, -1));
			lua_pop(lua, 1);
			// stack: table, key
		}


		if(insert)
		{
			push_nif_term(lua, tuple_terms[1], env, resource_type);
			// stack: table, key, value
			lua_rawset(lua, -3);
			// stack: table
			result = 1;
		}
		else
		{
			lua_pop(lua, 1);
		}
	}

	assert(lua_gettop(lua) == top);
	return result;
}


static inline int push_nif_atom(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	const int top = lua_gettop(lua);
	int result = 0;

	// max length for atoms is 255
    char atom[256] = { 0 }; 

    const int atom_length = enif_get_atom(env, message, atom, 256, ERL_NIF_LATIN1);
    if(0 < atom_length)
    {
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
	    if(strcmp(atom, "true") == 0)
	    {
	    	lua_pushboolean(lua, 1);
	    }	
	    else if(strcmp(atom, "false") == 0)
	    {
	    	lua_pushboolean(lua, 0);
	    }
	    else if(strcmp(atom, "nil") == 0)
	    {
	    	lua_pushnil(lua);
	    }
	    else
	    {
			// lua length doesn't include the \0
			lua_pushlstring(lua, (const char*)atom, atom_length - 1); 
	    }
		result = 1;
    }

	assert(lua_gettop(lua) == top + result);

	return result;
}

static inline int push_nif_binary(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	const int top = lua_gettop(lua);

	int result = 0;
	ErlNifBinary binary;
	if(enif_inspect_binary(env, message, &binary))
	{
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushlstring(lua, (const char*)binary.data, binary.size);
		result = 1;
	}

	assert(lua_gettop(lua) == top + result);

	return result;
}

static inline int push_nif_number(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	const int top = lua_gettop(lua);

	int result = 0;

    int intval;
    unsigned int uintval;
    long longval;
    unsigned long ulongval;
    double doubleval;

	luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
    if(enif_get_int(env, message, &intval))
    {
    	lua_pushnumber(lua, (double) intval);
        result = 1;
    }
    else if(enif_get_uint(env, message, &uintval))
    {
    	lua_pushnumber(lua, (double) uintval);
        result = 1;
    }
    else if(enif_get_long(env, message, &longval))
    {
    	lua_pushnumber(lua, (double) longval);
        result = 1;
    }
    else if(enif_get_ulong(env, message, &ulongval))
    {
    	lua_pushnumber(lua, (double) ulongval);
        result = 1;
    }
    else if(enif_get_double(env, message, &doubleval))
    {
    	lua_pushnumber(lua, (double) doubleval);
        result = 1;
    }

	assert(lua_gettop(lua) == top+result);

	return result;
}

static void push_nif_tuple(lua_State* lua, ERL_NIF_TERM tuple, ErlNifEnv* env, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);

	// TODO @@@ this function and push_nif_tuple are very very similar, refactor
	int map_count = 0;
	int array_count = 0;

	int arity = 0;
	const ERL_NIF_TERM* terms;

	if(enif_get_tuple(env, tuple, &arity, &terms))
	{
		int tuple_index = 0;
		for(; tuple_index < arity; ++tuple_index)
		{
			if(push_table_iskvp(env, terms[tuple_index], NULL))
			{
				++map_count;
			}
			else
			{
				++array_count;
			}
		}

		ERL_NIF_TERM array_items[array_count+map_count];

		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_createtable(lua, array_count, map_count);
		int index = 0;

		for(tuple_index = 0; tuple_index < arity; ++tuple_index)
		{
			if(!push_table_set(lua, env, terms[tuple_index], resource_type))
			{
				array_items[index++] = terms[tuple_index];
			}
		}

		push_table_append(lua, env, resource_type, array_items, index);
	}

	assert(lua_gettop(lua) == top+1);
}

static void push_nif_pid(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	const int top = lua_gettop(lua);

	luaL_checkstack(lua, 2, ERROR_STACK_MESSAGE);

	ErlNifPid *pid = (ErlNifPid *)lua_newuserdata(lua, sizeof(ErlNifPid));
	if(enif_get_local_pid(env, message, pid))
	{
		luaL_getmetatable(lua, TYPE_ERL_PID);
		lua_setmetatable(lua, -2);
	}

	assert(lua_gettop(lua) == top+1);
}

void terminator_tolua_luaaddress(lua_State* lua, void* reference)
{
	assert(NULL != lua);
	assert(NULL != reference);
	const int top = lua_gettop(lua);

	mailbox_address_ptr address = (mailbox_address_ptr)lua_newuserdata(lua, sizeof(struct mailbox_address));
	memset(address, 0, sizeof(struct mailbox_address));

	address->state_work = reference;
	address->ref_type = WEAK_REF;

	luaL_getmetatable(lua, TYPE_LUA_ADDRESS);
	lua_setmetatable(lua, -2);

	assert(lua_gettop(lua) == top+1);
}

static void push_nif_ref(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	(void)env; // unused
	const int top = lua_gettop(lua);

	luaL_checkstack(lua, 2, ERROR_STACK_MESSAGE);

	ERL_NIF_TERM *ref = (ERL_NIF_TERM *)lua_newuserdata(lua, sizeof(ERL_NIF_TERM));
	*ref = message;
	luaL_getmetatable(lua, TYPE_ERL_REF);
	lua_setmetatable(lua, -2);

	assert(lua_gettop(lua) == top+1);
}

static void push_nif_list(lua_State* lua, ERL_NIF_TERM list, ErlNifEnv* env, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);

	int map_count = 0;
	int array_count = 0;
	if(enif_is_list(env, list))
	{
		ERL_NIF_TERM head;
		ERL_NIF_TERM tail = list;
		while(enif_get_list_cell(env, tail, &head, &tail))
		{
			if(push_table_iskvp(env, head, NULL))
			{
				++map_count;
			}
			else
			{
				++array_count;
			}
		}

		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_createtable(lua, array_count, map_count);

		ERL_NIF_TERM array_items[array_count + map_count];

		tail = list;
		int index = 0;
		while(enif_get_list_cell(env, tail, &head, &tail))
		{
			if(!push_table_set(lua, env, head, resource_type))
			{
				array_items[index++] = head;
			}
		}

		push_table_append(lua, env, resource_type, array_items, index);
	}

	assert(lua_gettop(lua) == top+1);
}


static void push_nif_term(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);

	if(enif_is_atom(env, message))
	{
		push_nif_atom(lua, message, env);
	}
	else if(enif_is_binary(env, message))
	{
		// TODO @@@ binary also seems to be the custom types
		// that erlang makes. This may be OK, but maybe we should
		// think about putting a special type for them in lua
		push_nif_binary(lua, message, env);
	}
	else if(enif_is_list(env, message))
	{
		if(enif_is_empty_list(env, message))
		{
			luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
			lua_newtable(lua);
		}
		else
		{
			// TODO @@@ try to send it as an IO list first and
			// if that fails send it as a regular list
			push_nif_list(lua, message, env, resource_type);
		}
	}
	else if(enif_is_tuple(env, message))
	{	
		push_nif_tuple(lua, message, env, resource_type);
	}
	else if(enif_is_pid(env, message))
	{
		push_nif_pid(lua, message, env);
	}
	else if(enif_is_ref(env, message))
	{
		push_nif_ref(lua, message, env);
	}
	else if(enif_is_exception(env, message))
	{
		//printf("#exception\n");
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushliteral(lua, "sending an exception is not supported");
	}
	else if(enif_is_fun(env, message))
	{
		//printf("#fun\n");
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushliteral(lua, "sending a function reference is not supported");
	}
	else if(enif_is_port(env, message))
	{
		//printf("#port\n");
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushliteral(lua, "sending a port is not supported");
	}
	else
	{
		// thank you r14 -- must be a number
		push_nif_number(lua, message, env);
	}
	assert(lua_gettop(lua) == top+1);
}

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);
	push_nif_term(lua, message, env, resource_type);
	assert(lua_gettop(lua) == top+1);
}


// takes the first term off of the lua stack and return it as an
// erlang term in the state 'env'.
static int terminator_toerl_core(lua_State* lua, ERL_NIF_TERM *result, ErlNifEnv* env, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);

	switch(lua_type(lua, top))
	{
		case LUA_TNIL:
		{
			*result = enif_make_atom(env, "nil");
			lua_pop( lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TNUMBER:
		{
			const lua_Number number = luaL_checknumber(lua, top);
			*result = enif_make_double(env, (double)number);
			lua_pop( lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TBOOLEAN:
		{
			const int truefalse = lua_toboolean(lua, top);
			*result = truefalse ? enif_make_atom(env, "true") : enif_make_atom(env, "false");
			lua_pop( lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TSTRING:
		{
			// get the lua string
			size_t string_len;
			const char * lua_string = lua_tolstring (lua, top, &string_len);

			// make space in erlang for it
			ErlNifBinary binary;
			if(enif_alloc_binary(string_len, &binary))
			{
				// clean it
				memset(binary.data, 0, binary.size);

				// copy it over
				memcpy(binary.data, lua_string, string_len);

				*result = enif_make_binary(env, &binary);
			}
			else
			{
				luaL_error (lua, "could not convert lua string");
			}
			lua_pop( lua, 1);

			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TTABLE:
		{
			size_t table_size = 0;

			// table is at the top of the stack
			lua_pushnil(lua);  // nil as the first key
			while (lua_next(lua, top) != 0) {
				++table_size;
				lua_pop(lua, 1);
			}

			// make sure we can grow the stack
			luaL_checkstack(lua, 2, ERROR_STACK_MESSAGE);

			ERL_NIF_TERM *new_table = (ERL_NIF_TERM*) node_alloc(table_size * sizeof(ERL_NIF_TERM));
			ERL_NIF_TERM *next_cell = new_table;

			// table is at the top of the stack
			lua_pushnil(lua);  // nil as the first key
			while (lua_next(lua, top) != 0) {
				// uses 'key' (at index -2) and 'value' (at index -1)

				// remove 'value', keeps 'key' for next iteration
				ERL_NIF_TERM tuple_value;
				terminator_toerl_core(lua, &tuple_value, env, resource_type);

				ERL_NIF_TERM tuple_key;
				lua_pushvalue( lua, -1);
				terminator_toerl_core(lua, &tuple_key, env, resource_type);

				*next_cell = enif_make_tuple2(env, tuple_key, tuple_value);
				next_cell++;
			}

			if(NULL != new_table)
			{
				*result = enif_make_list_from_array(env, new_table, table_size);
				node_free(new_table);
			}

			lua_pop( lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TUSERDATA:
		{
			// add metatable to stack
			if(lua_getmetatable (lua, top))
			{

				// put the pid metatable onto the stack
				// compare the two metatables
				luaL_getmetatable(lua, TYPE_ERL_PID);
				if(lua_compare(lua, -1, -2, LUA_OPEQ))
				{
					const ErlNifPid* userdata = (const ErlNifPid*) lua_touserdata(lua, top);
					*result = enif_make_pid(env, userdata);
					lua_pop(lua, 3);
					assert(lua_gettop(lua) == top-1);
					return 1;
				}
				// pop the pid metatable
				lua_pop(lua, 1);			

				// push the ref metatable
				luaL_getmetatable(lua, TYPE_ERL_REF);
				if(lua_compare(lua, -1, -2, LUA_OPEQ))
				{
					ERL_NIF_TERM* nif_ptr = (ERL_NIF_TERM*) lua_touserdata(lua, top);
					*result = (*nif_ptr);
					lua_pop(lua, 3);
					assert(lua_gettop(lua) == top-1);
					return 1;
				}
				lua_pop(lua, 1);
				
				// pop the ref metatable
				luaL_getmetatable(lua, TYPE_LUA_ADDRESS);
				if(lua_compare(lua, -1, -2, LUA_OPEQ))
				{
					mailbox_address_ptr address = (mailbox_address_ptr) lua_touserdata(lua, top);

					assert(NULL != address);
					state_work_ptr state_work = address->state_work;
					assert(NULL != state_work);
					void* resource;
					(*result) = state_make_resource( env, &resource, resource_type, state_work, WEAK_REF);
					assert(NULL != resource);
					lua_pop(lua, 3);

					assert(lua_gettop(lua) == top-1);
					return 1;
				}

				// pop the metatable
				lua_pop(lua, 1);
			}

			lua_pop( lua, 2);
			*result = enif_make_atom(env, "unknown_lua_userdata");
			
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TLIGHTUSERDATA:
		{
			*result = enif_make_atom(env, "lua_lightuserdata_notsupported");
			lua_pop(lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TTHREAD:
		{
			*result = enif_make_atom(env, "lua_thread_notsupported");
			lua_pop(lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
		case LUA_TFUNCTION:
		{
			*result = enif_make_atom(env, "lua_function_notsupported");
			lua_pop(lua, 1);
			assert(lua_gettop(lua) == top-1);
			return 1;
			break;
		}
	}
	assert(lua_gettop(lua) == top-1);
	return 0;
}

int terminator_toerl(lua_State* lua, ERL_NIF_TERM *result, ErlNifEnv* env, ErlNifResourceType* resource_type)
{
	const int top = lua_gettop(lua);

	int retval = terminator_toerl_core(lua, result, env, resource_type);

	assert(lua_gettop(lua) == top-retval);
	return retval;
}

void* terminator_lua_checkpid(lua_State* lua, int index)
{
	return luaL_checkudata(lua, index, TYPE_ERL_PID);
}

static int lua_address_gc (lua_State *lua)
{
	const int top = lua_gettop(lua);
	
	mailbox_address_ptr address = (mailbox_address_ptr) luaL_checkudata(lua, 1, TYPE_LUA_ADDRESS);
	luaL_argcheck( lua, address, 1, "lua address expected");

	const state_work_ptr state_work = address->state_work;
	assert(NULL != state_work);

	lua_pushvalue(lua, lua_upvalueindex(1));
	const state_work_ptr state_work_self = (state_work_ptr) lua_touserdata(lua, -1);
	lua_pop(lua, 1);
	assert(NULL != state_work_self);

	if(STRONG_REF == address->ref_type)
	{
		state_work_decref( state_work );
	}

	assert(lua_gettop(lua) == top);

	return 0;
}

void terminator_create_types(lua_State* lua, void* state_work)
{
	int top = lua_gettop(lua);

	// create metatable and metatable.__index = metatable
	luaL_newmetatable( lua, TYPE_ERL_PID);
	lua_pushliteral( lua, "__index");
	lua_pushvalue( lua, -2);
	lua_rawset( lua, -3);
	lua_pop( lua, 1);	

	luaL_newmetatable( lua, TYPE_ERL_REF);
	lua_pushliteral( lua, "__index");
	lua_pushvalue( lua, -2);
	lua_rawset( lua, -3);
	lua_pop( lua, 1);	

	luaL_newmetatable( lua, TYPE_LUA_ADDRESS);
	lua_pushliteral( lua, "__index");
	lua_pushvalue( lua, -2);
	lua_rawset( lua, -3);

//	lua_pushstring( lua, "class");
//	lua_pushliteral( lua, TYPE_LUA_ADDRESS);
	lua_pushliteral( lua, "__gc");
	lua_pushlightuserdata(lua, state_work);
	lua_pushcclosure(lua, lua_address_gc, 1);
	lua_rawset( lua, -3);

	// lua_rawset( lua, -3);
	lua_pop(lua, 1);

	assert(lua_gettop(lua) == top);
}

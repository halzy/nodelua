#include "terminator.h"

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <string.h>

#define TYPE_PID "mailbox.id"
#define TYPE_REF "nodelua.terminator.ref"

static const char* types[] = 
{
	TYPE_PID,
	TYPE_REF,
	0
};

#define ERROR_STACK_MESSAGE "Could not grow stack large enough to read message."

static void push_nif_term(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env);

static inline int push_nif_atom(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
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

	return result;
}

static inline int push_nif_binary(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	int result = 0;
	ErlNifBinary binary;
	if(enif_inspect_binary(env, message, &binary))
	{
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushlstring(lua, (const char*)binary.data, binary.size);
		result = 1;
	}
	return result;
}

static inline int push_nif_number(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
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

	return result;
}

static void push_nif_tuple(lua_State* lua, ERL_NIF_TERM tuple, ErlNifEnv* env)
{
	int map_count = 0;
	int array_count = 0;

	int arity = 0;
	const ERL_NIF_TERM* terms;

	if(enif_get_tuple(env, tuple, &arity, &terms))
	{
		int tuple_index = 0;
		for(; tuple_index < arity; ++tuple_index)
		{
			int tuple_arity = 0;
			const ERL_NIF_TERM* tuple_terms;
			if(enif_get_tuple(env, terms[tuple_index], &tuple_arity, &tuple_terms))
			{
				if(tuple_arity == 2 && 
						(enif_is_atom(env, tuple_terms[0]) || 
						enif_is_binary(env, tuple_terms[0]) || 
						enif_is_list(env, tuple_terms[0])) 
					)
				{
					++map_count;
					continue;
				}
			}
			++array_count;
		}

		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_createtable(lua, array_count, map_count);
		int table_index;
		for(tuple_index = 1; tuple_index <= arity; ++tuple_index)
		{
			int tuple_arity = 0;
			const ERL_NIF_TERM* tuple_terms;

			if(enif_get_tuple(env, terms[tuple_index], &tuple_arity, &tuple_terms))
			{
				if(tuple_arity == 2 && 
						(enif_is_atom(env, tuple_terms[0]) || 
						enif_is_binary(env, tuple_terms[0]) || 
						enif_is_list(env, tuple_terms[0])) 
					)
				{
					push_nif_term(lua, tuple_terms[0], env);
					push_nif_term(lua, tuple_terms[1], env);
					lua_rawset(lua, -3);
					continue;
				}
			}

			push_nif_term(lua, terms[tuple_index], env);
			lua_rawseti(lua, -2, table_index++);
		}
	}
}

static void push_nif_pid(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	luaL_checkstack(lua, 2, ERROR_STACK_MESSAGE);

	ErlNifPid *pid = (ErlNifPid *)lua_newuserdata(lua, sizeof(ErlNifPid));
	if(enif_get_local_pid(env, message, pid))
	{
		luaL_getmetatable(lua, TYPE_PID);
		lua_setmetatable(lua, -2);
	}
}

static void push_nif_ref(lua_State* lua, ERL_NIF_TERM message)
{
	luaL_checkstack(lua, 2, ERROR_STACK_MESSAGE);

	ERL_NIF_TERM *ref = (ERL_NIF_TERM *)lua_newuserdata(lua, sizeof(ERL_NIF_TERM));
	*ref = message;
	luaL_getmetatable(lua, TYPE_REF);
	lua_setmetatable(lua, -2);
}

static void push_nif_list(lua_State* lua, ERL_NIF_TERM list, ErlNifEnv* env)
{
	int map_count = 0;
	int array_count = 0;
	if(enif_is_list(env, list))
	{
		ERL_NIF_TERM head;
		ERL_NIF_TERM tail = list;
		while(enif_get_list_cell(env, tail, &head, &tail))
		{
			int tuple_arity = 0;
			const ERL_NIF_TERM* tuple_terms;
			if(enif_get_tuple(env, head, &tuple_arity, &tuple_terms))
			{
				if(tuple_arity == 2 && 
						(enif_is_atom(env, tuple_terms[0]) || 
						enif_is_binary(env, tuple_terms[0]) || 
						enif_is_list(env, tuple_terms[0])) 
					)				
				{
					++map_count;
					continue;
				}
			}
			++array_count;
		}

		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_createtable(lua, array_count, map_count);

		tail = list;
		int index = 1;
		while(enif_get_list_cell(env, tail, &head, &tail))
		{
			int tuple_arity = 0;
			const ERL_NIF_TERM* tuple_terms;

			if(enif_get_tuple(env, head, &tuple_arity, &tuple_terms))
			{
				if(tuple_arity == 2 && 
						(enif_is_atom(env, tuple_terms[0]) || 
						enif_is_binary(env, tuple_terms[0]) || 
						enif_is_list(env, tuple_terms[0])) 
					)				{
					push_nif_term(lua, tuple_terms[0], env);
					push_nif_term(lua, tuple_terms[1], env);
					lua_rawset(lua, -3);
					continue;
				}
			}

			push_nif_term(lua, head, env);
			lua_rawseti(lua, -2, index++);
		}
	}
}


static void push_nif_term(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
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
			push_nif_list(lua, message, env);
		}
	}
	else if(enif_is_number(env, message))
	{
		push_nif_number(lua, message, env);
	}
	else if(enif_is_tuple(env, message))
	{
		push_nif_tuple(lua, message, env);
	}
	else if(enif_is_pid(env, message))
	{
		push_nif_pid(lua, message, env);
	}
	else if(enif_is_ref(env, message))
	{
		push_nif_ref(lua, message);
	}
	else if(enif_is_exception(env, message))
	{
		printf("#exception\n");
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushliteral(lua, "sending an exception is not supported");
	}
	else if(enif_is_fun(env, message))
	{
		printf("#fun\n");
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushliteral(lua, "sending a function reference is not supported");
	}
	else if(enif_is_port(env, message))
	{
		printf("#port\n");
		luaL_checkstack(lua, 1, ERROR_STACK_MESSAGE);
		lua_pushliteral(lua, "sending a port is not supported");
	}
}

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	push_nif_term(lua, message, env);
}


// takes the first term off of the lua stack and return it as an
// erlang term in the state 'env'.
static int terminator_toerl_core(lua_State* lua, ERL_NIF_TERM *result, ErlNifEnv* env)
{
	const int top = lua_gettop(lua);

	switch(lua_type(lua, top))
	{
		case LUA_TNIL:
		{
			*result = enif_make_atom(env, "nil");
			return 1;
			break;
		}
		case LUA_TNUMBER:
		{
			const lua_Number number = luaL_checknumber(lua, top);
			*result = enif_make_double(env, (double)number);
			return 1;
			break;
		}
		case LUA_TBOOLEAN:
		{
			const int truefalse = lua_toboolean(lua, top);
			*result = truefalse ? enif_make_atom(env, "true") : enif_make_atom(env, "false");
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

			ERL_NIF_TERM *new_table = (ERL_NIF_TERM*) enif_alloc(table_size * sizeof(ERL_NIF_TERM));
			ERL_NIF_TERM *next_cell = new_table;

			// table is at the top of the stack
			lua_pushnil(lua);  // nil as the first key
			while (lua_next(lua, top) != 0) {
				// uses 'key' (at index -2) and 'value' (at index -1)
				ERL_NIF_TERM tuple_value;
				terminator_toerl_core(lua, &tuple_value, env);

				// remove 'value', keeps 'key' for next iteration
				lua_pop(lua, 1);
				ERL_NIF_TERM tuple_key;
				terminator_toerl_core(lua, &tuple_key, env);

				*next_cell = enif_make_tuple2(env, tuple_key, tuple_value);
				next_cell++;
			}

			if(NULL != new_table)
			{
				*result = enif_make_list_from_array(env, new_table, table_size);
				enif_free(new_table);
			}

			return 1;
			break;
		}
		case LUA_TUSERDATA:
		{
			// TODO @@@ checkpid will barf if it is a ref
			void* userdata = terminator_lua_checkpid(lua, top);
			if(NULL != userdata)
			{
				*result = enif_make_pid(env, (const ErlNifPid*) userdata);
				return 1;
			}

			userdata = luaL_checkudata(lua, top, TYPE_REF);
			if(NULL != userdata)
			{			
				return 1;
			}

			break;
		}
		case LUA_TLIGHTUSERDATA:
		{
			*result = enif_make_atom(env, "lua_lightuserdata_notsupported");
			return 1;
			break;
		}
		case LUA_TTHREAD:
		{
			*result = enif_make_atom(env, "lua_thread_notsupported");
			return 1;
			break;
		}
		case LUA_TFUNCTION:
		{
			*result = enif_make_atom(env, "lua_function_notsupported");
			return 1;
			break;
		}
	}
	return 0;
}

int terminator_toerl(lua_State* lua, ERL_NIF_TERM *result, ErlNifEnv* env)
{
	int retval = terminator_toerl_core(lua, result, env);
	lua_pop(lua, 1);
	return retval;
}

void* terminator_lua_checkpid(lua_State* lua, int index)
{
	return luaL_checkudata(lua, index, TYPE_PID);
}

void terminator_create_types(lua_State* lua)
{
	// create metatable and metatable.__index = metatable
	int i = 0;
	for(i = 0; types[i]; i++)
	{
		luaL_newmetatable(lua, types[i]);
		lua_pop(lua, 1);	
	}
}

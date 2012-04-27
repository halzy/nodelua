#include "terminator.h"

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <string.h>

#define TYPE_PID "nodelua.terminator.pid"

static void push_nif_term(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env);


static inline int push_nif_atom(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	int result = 0;
	unsigned int atom_length;
	if(enif_get_atom_length(env, message, &atom_length, ERL_NIF_LATIN1))
	{
		++atom_length; // add a space for terminating null
		char* atom = (char*) enif_alloc(atom_length);
		memset(atom, 0, atom_length);
		if(NULL != atom)
		{
			if(enif_get_atom(env, message, atom, atom_length, ERL_NIF_LATIN1))
			{
				lua_pushlstring(lua, (const char*)atom, atom_length);
				result = 1;
			}
			enif_free(atom);
		}
	}
	return result;
}

static inline int push_nif_binary(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	int result = 0;
	ErlNifBinary binary;
	if(enif_inspect_binary(env, message, &binary))
	{
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
				if(tuple_arity == 2 && enif_is_atom(env, tuple_terms[0]))
				{
					++map_count;
					continue;
				}
			}
			++array_count;
		}

		lua_createtable(lua, array_count, map_count);
		int table_index = 0;
		for(tuple_index = 0; tuple_index < arity; ++tuple_index)
		{
			int tuple_arity = 0;
			const ERL_NIF_TERM* tuple_terms;

			if(enif_get_tuple(env, terms[tuple_index], &tuple_arity, &tuple_terms))
			{
				if(tuple_arity == 2 && enif_is_atom(env, tuple_terms[0]))
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
				if(tuple_arity == 2 && enif_is_atom(env, tuple_terms[0]))
				{
					++map_count;
					continue;
				}
			}
			++array_count;
		}

		lua_createtable(lua, array_count, map_count);

		tail = list;
		int index = 0;
		while(enif_get_list_cell(env, tail, &head, &tail))
		{
			int tuple_arity = 0;
			const ERL_NIF_TERM* tuple_terms;

			if(enif_get_tuple(env, head, &tuple_arity, &tuple_terms))
			{
				if(tuple_arity == 2 && enif_is_atom(env, tuple_terms[0]))
				{
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
	else if(enif_is_pid(env, message))
	{
		printf("#pid\n");
		lua_pushliteral(lua, "unknown");
	}
	else if(enif_is_tuple(env, message))
	{
		printf("#tuple\n");
		push_nif_tuple(lua, message, env);
	}
	else if(enif_is_exception(env, message))
	{
		printf("#exception\n");
		lua_pushliteral(lua, "unknown");
	}
	else if(enif_is_fun(env, message))
	{
		printf("#fun\n");
		lua_pushliteral(lua, "unknown");
	}
	else if(enif_is_port(env, message))
	{
		printf("#port\n");
		lua_pushliteral(lua, "unknown");
	}
	else if(enif_is_ref(env, message))
	{
		printf("#ref\n");
		lua_pushliteral(lua, "unknown");
	}
}

void terminator_tolua(lua_State* lua, ERL_NIF_TERM message, ErlNifEnv* env)
{
	push_nif_term(lua, message, env);
	/*
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
	*/
}

static const char* types[] = 
{
	TYPE_PID,
	0
};

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

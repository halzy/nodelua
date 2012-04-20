#include "erllua.h"
#include "nl_util.h"

#include <erl_nif.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <string.h>

typedef struct erllua_s
{
    lua_State *lua;
    lua_State *coroutine;
    int state;
} erllua_t;

typedef struct lua_input_script
{
  const char *data;
  const size_t size;
  int done;
} lua_input_script;


void erllua_destroy(erllua_ptr erllua)
{
  printf("erllua_destroy called with %p\n", erllua);
  // TODO is there more to clean? 
  // Like gathering a result and sending it back?
  lua_close(erllua->lua);
}


int erllua_run(erllua_ptr erllua)
{
  int result = lua_resume(erllua->coroutine, 0);
  switch(result)
  {
    case 0:
    {
      erllua->state = ERLLUA_DONE;
      break;
    }
    case LUA_YIELD:
    {
      erllua->state = ERLLUA_YIELD;
      break;
    }
    default:
    {
      erllua->state = ERLLUA_DONE;      
      printf("Lua Error: %s", lua_tostring(erllua->lua, 1));
      lua_pop(erllua->lua, 1);
      break;
    }
  }

  return erllua->state;
}

static const char *read_input_script(lua_State *env, void *user_data, size_t *size)
{
  lua_input_script *input = (lua_input_script *) user_data;

  // return NULL if we are done reading
  if(input->done)
    return NULL;

  // give it all we've got and signal that we are done
  *size = input->size;
  input->done = 1;

  return input->data;
}

// returns {ok, handle} and sets erllua_result or {error, {kind, message}} and erllua_result is NULL
ERL_NIF_TERM erllua_create(ErlNifEnv* env, ErlNifResourceType* erllua_type, const char* data, const unsigned size, const char* name, erllua_ptr *erllua_result)
{
  ERL_NIF_TERM result;

  (*erllua_result) = NULL; // default to 'error'

  // create a new lua state and only attach it to erlang on success
  lua_State *lua = luaL_newstate();
  if(NULL == lua)
  {
    return make_error_tuple(env, ATOM_LUA, "Could not create a new lua state");
  }

  lua_State *coroutine = lua_newthread(lua);

  luaL_openlibs(coroutine);

  lua_input_script input_script = { .data = data, .size = size, .done = 0 };

  // Load the command and try to execute it... we don't want to hold onto
  // the binary longer than this, so give it to lua and store it as lua
  // runtime script
  if (0 == lua_load(coroutine, read_input_script, &input_script, name))
  {
    // since everythin was initialized with lua, bind it to the erlang 
    // system by creating the registered type and attaching the lua state
    erllua_ptr erllua = enif_alloc_resource(erllua_type, sizeof(erllua_t));
    if(NULL != erllua)
    {
      memset(erllua, '\0', sizeof(erllua_t));

      // associate lua with the return value
      erllua->state = ERLLUA_START;
      erllua->lua = lua;
      erllua->coroutine = coroutine;

      ERL_NIF_TERM erllua_handle = enif_make_resource(env, erllua);
      // release our reference to the new erlang variable
      enif_release_resource(erllua);

      result = enif_make_tuple2(env, ATOM_OK, erllua_handle);
      *erllua_result = erllua;
    }
    else
    {
      result = make_error_tuple(env, ATOM_MEMORY, "enif_alloc_resource returned NULL");
      lua_close(lua);
    }
  }
  else
  {
    result = make_error_tuple(env, ATOM_LUA, lua_tostring(lua, -1));
    lua_pop(lua, 1); /* pop error message from the stack */
    lua_close(lua); 
  }

  return result;
}

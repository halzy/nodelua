#include "erl_nif.h"
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <string.h>
#include <assert.h>

static ErlNifResourceType* luastate_RESOURCE = NULL;

/* TODO List:
Send results back to erlang
Ability to load a file instead of a buffer
Create single thread to work the lua states
Message Queue system so we can pass messages
Create thread pool to work the lua states
*/

typedef struct
{
  // a list of lua_States
  // a list of worker threads
} nif_state;

typedef struct
{
    lua_State *lua;
} luastate_handle;

typedef struct lua_input_script
{
  const char *data;
  const size_t size;
  int done;
} lua_input_script;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_MEMORY;
static ERL_NIF_TERM ATOM_LUA;

// Prototypes
static ERL_NIF_TERM nodelua_run_core(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nodelua_send_core(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"run_core", 1, nodelua_run_core},
    {"send_core", 2, nodelua_send_core}
};


// Utility function for making error result terms
ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char* message)
{
    ERL_NIF_TERM reason = enif_make_string(env, message, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, ATOM_ERROR, enif_make_tuple2(env, error, reason));
}

const char *read_input_script(lua_State *env, void *user_data, size_t *size)
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

static ERL_NIF_TERM nodelua_run_core_script(ErlNifEnv* env, const char* data, const unsigned size)
{
  ERL_NIF_TERM result;

  // create a new lua state and only attach it to erlang on success
  lua_State *lua = luaL_newstate();
  if(NULL != lua)
  {
    luaL_openlibs(lua);
    // || lua_pcall(lua, 0, 0, 0);

    lua_input_script input_script = { .data = data, .size = size, .done = 0 };

    // Load the command and try to execute it...
    if (0 == lua_load(lua, read_input_script, &input_script, "test script"))
    {
      // since everythin was initialized with lua, bind it to the erlang 
      // system by creating the registered type and attaching the lua state

      // allocate our lua type
      luastate_handle* handle = enif_alloc_resource(luastate_RESOURCE, sizeof(luastate_handle));
      if(NULL != handle)
      {
        // associate lua with the return value
        handle->lua = lua;
        ERL_NIF_TERM lua_handle = enif_make_resource(env, handle);
        result = enif_make_tuple2(env, ATOM_OK, lua_handle);

        // release our reference to the new variable
        enif_release_resource(handle);
      }
      else
      {
        result = make_error_tuple(env, ATOM_MEMORY, "enif_alloc_resource returned NULL");
      }
    }
    else
    {
      result = make_error_tuple(env, ATOM_LUA, lua_tostring(lua, -1));
      lua_pop(lua, 1); /* pop error message from the stack */
      lua_close(lua); 
    }
  }
  else
  {
    result = make_error_tuple(env, ATOM_LUA, "Could not create a new lua state");
  }

  return result;
}


static ERL_NIF_TERM nodelua_run_core_binary(ErlNifEnv* env, ERL_NIF_TERM bin_term)
{
  ErlNifBinary binary;
  assert(enif_inspect_binary(env, bin_term, &binary));

  return nodelua_run_core_script(env, (const char *)binary.data, binary.size);
}

static ERL_NIF_TERM nodelua_run_core_list(ErlNifEnv* env, ERL_NIF_TERM list)
{
  ErlNifBinary binary;
  assert(enif_inspect_iolist_as_binary(env, list, &binary));

  return nodelua_run_core_script(env, (const char *)binary.data, binary.size);
}

/*
static ERL_NIF_TERM nodelua_run_core_list(ErlNifEnv* env, ERL_NIF_TERM list)
{
  ERL_NIF_TERM result;

  // check if a string was given and allocate the length of the string name
  unsigned script_length = 0;

  // by this time we should have already checked that is is as list
  assert(enif_get_list_length(env, list, &script_length));
  
  ++script_length; // add an extra space for a safety termination \0
  unsigned char *script = enif_alloc(script_length);

  if(NULL != script)
  {
    memset(script, '\0', script_length);

    // load the string into the script variable
    enif_get_string(env, list, script, script_length, ERL_NIF_LATIN1);

    result = nodelua_run_core_script(env, script, script_length);
  }
  else
  {
    result = make_error_tuple(env, ATOM_MEMORY, "Memory allocation failed");
  }

  return result;
}
*/


static ERL_NIF_TERM nodelua_run_core(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;

  const ERL_NIF_TERM script = argv[0];

  if(enif_is_binary(env, script))
  {
    result = nodelua_run_core_binary(env, script);
  }
  else if(enif_is_list(env, script))
  {
    result = nodelua_run_core_list(env, script);
  }
  else
  {
    result = enif_make_badarg(env);    
  }

  return result;
}

static ERL_NIF_TERM nodelua_send_core(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return ATOM_OK;
}

static void luastate_resource_cleanup(ErlNifEnv* env, void* arg)
{
  /* Delete any dynamically allocated memory stored in nodelua_handle */
  luastate_handle* handle = (luastate_handle*)arg;
  lua_close(handle->lua);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  printf("on_load called with %p\n", *priv_data);

  ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
  ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                   "nodelua_luastate_RESOURCE",
                                                   &luastate_resource_cleanup,
                                                   flags, NULL);
  if (rt == NULL)
      return -1;

  luastate_RESOURCE = rt;

  ATOM_OK = enif_make_atom(env, "ok");
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_MEMORY = enif_make_atom(env, "memory");
  ATOM_LUA = enif_make_atom(env, "lua");

  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
  printf("on_unload called with %p\n", priv_data);
}

ERL_NIF_INIT(nodelua, nif_funcs, &on_load, NULL, NULL, &on_unload);

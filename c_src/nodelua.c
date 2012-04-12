#include "erl_nif.h"
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <string.h>

static ErlNifResourceType* luastate_RESOURCE = NULL;

typedef struct
{
    lua_State *lua;
} luastate_handle;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_MEMORY;
static ERL_NIF_TERM ATOM_LUA;

// Prototypes
static ERL_NIF_TERM nodelua_run(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nodelua_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"run", 1, nodelua_run},
    {"send", 2, nodelua_send}
};


// Utility function for making error result terms
ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char* message)
{
    ERL_NIF_TERM reason = enif_make_string(env, message, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, ATOM_ERROR, enif_make_tuple2(env, error, reason));
}


static ERL_NIF_TERM nodelua_run(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;

  // check if a string was given and allocate the length of the string name
  unsigned script_name_length = 0;
  if(enif_get_list_length(env, argv[0], &script_name_length))
  {
    char *script_name = enif_alloc(script_name_length);
    memset(script_name, '\0', script_name_length);

    if(NULL != script_name)
    {
      // load the string into the script_name variable
      enif_get_string(env, argv[0], script_name, script_name_length, ERL_NIF_LATIN1);

      // create a new lua state and only attach it to erlang on success
      lua_State *lua = luaL_newstate();

      if(NULL != lua)
      {
        luaL_openlibs(lua);
        int error = luaL_loadbuffer(lua, script_name, strlen(script_name), "line") || lua_pcall(lua, 0, 0, 0);
        if (error) 
        {
          result = make_error_tuple(env, ATOM_LUA, lua_tostring(lua, -1));
          lua_pop(lua, 1); /* pop error message from the stack */
          lua_close(lua); 
        }
        else
        {
          // since everythin was initialized with lua, bind it to the erlang 
          // system by creating the registered type and attaching the lua state

          // allocate our lua type
          luastate_handle* handle = enif_alloc_resource(luastate_RESOURCE, sizeof(luastate_handle));

          // associate lua with the return value
          handle->lua = lua;
          ERL_NIF_TERM lua_handle = enif_make_resource(env, handle);
          result = enif_make_tuple2(env, ATOM_OK, lua_handle);

          // release our reference to the new variable
          enif_release_resource(handle);
        }
      }
    }
    else
    {
      result = make_error_tuple(env, ATOM_MEMORY, "Memory allocation failed");
    }
  }
  else
  {
    result = enif_make_badarg(env);
  }

  return result;
}

static ERL_NIF_TERM nodelua_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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

ERL_NIF_INIT(nodelua, nif_funcs, &on_load, NULL, NULL, NULL);

#include "nl_util.h"
#include "state.h"

#include <erl_nif.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <string.h>
#include <assert.h>

/* TODO List:
Send results back to erlang
Message Queue system so we can pass messages
Create thread pool to work the lua states
*/

// Prototypes
static ERL_NIF_TERM nodelua_run_core(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nodelua_send_core(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"run_core", 1, nodelua_run_core},
    {"send_core", 2, nodelua_send_core}
};

// returns {ok, resource} or {error, {kind, message}}
static ERL_NIF_TERM nodelua_run_core(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM result;

  const ERL_NIF_TERM script = argv[0];
  ErlNifBinary binary;

  if(enif_inspect_iolist_as_binary(env, script, &binary))
  {
    // allocate our lua type
    result = state_add_script(env, (const char *)binary.data, binary.size, "script");
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

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  nl_util_init_atoms(env);

  (*priv_data) = state_create(env);

  state_add_worker(env);

  return (NULL == *priv_data) ? 1 : 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
  state_destroy(env);
}

ERL_NIF_INIT(nodelua, nif_funcs, on_load, NULL, NULL, on_unload);

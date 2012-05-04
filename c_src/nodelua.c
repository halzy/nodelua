#include "nl_util.h"
#include "state.h"

#include <erl_nif.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <string.h>
#include <assert.h>

/* TODO List:
Message Queue system so we can pass messages
Embed a function in lua to get the terms from the message queue.
Embed a function in lua so it can send messages.
Send lua error results back to erlang
Create thread pool to work the lua states
Interrupt threads that are taking too much time
  may need no move away from using erlang thread API and using posix threads
  pthread_t pthread_self(void)

socket reference:
  https://bitbucket.org/liamdevine/luasocket_ipv6_lua_51_52/src/538dcef303e2/src/socket_scripts.c
translation ref:
  https://github.com/davisp/emonk/blob/master/c_src/to_js.c

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
  ERL_NIF_TERM result;

  if(2 == argc)
  {
    const ERL_NIF_TERM resource = argv[0];
    const ERL_NIF_TERM message = argv[1];

    if(state_send_message(env, resource, message))
    {
      result = ATOM_OK;
    }
    else
    {
      result = enif_make_badarg(env);
    }    
  }
  else
  {
    result = enif_make_badarg(env);
  }

  return result;
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
  (void) priv_data; // unused - the state type/data is hidden in env
  state_destroy(env);
}

ERL_NIF_INIT(nodelua, nif_funcs, on_load, NULL, NULL, on_unload);

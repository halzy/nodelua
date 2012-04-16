#include "nl_util.h"

#include <erl_nif.h>

// Utility function for making error result terms
ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char* message)
{
    ERL_NIF_TERM reason = enif_make_string(env, message, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, ATOM_ERROR, enif_make_tuple2(env, error, reason));
}

void nl_util_init_atoms(ErlNifEnv* env)
{
  ATOM_OK = enif_make_atom(env, "ok");
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_MEMORY = enif_make_atom(env, "memory");
  ATOM_LUA = enif_make_atom(env, "lua");
}

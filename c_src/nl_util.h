#ifndef NL_UTIL
#define NL_UTIL

#include <erl_nif.h>

#if (ERL_NIF_MAJOR_VERSION > 2) || (ERL_NIF_MAJOR_VERSION == 2 && ERL_NIF_MINOR_VERSION >= 3)
#define erl_is_number(env, term) enif_is_number(env, term)
#else
#define erl_is_number(env, term) !(enif_is_binary(env, term) || enif_is_list(env, term) || enif_is_tuple(env, term))
#endif

// Atoms (initialized in on_load)
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_MEMORY;
ERL_NIF_TERM ATOM_LUA;

ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char* message);
void nl_util_init_atoms(ErlNifEnv* env);

#endif

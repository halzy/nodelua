#ifndef NL_UTIL
#define NL_UTIL

#include <erl_nif.h>

// Atoms (initialized in on_load)
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_MEMORY;
ERL_NIF_TERM ATOM_LUA;

ERL_NIF_TERM make_error_tuple(ErlNifEnv* env, ERL_NIF_TERM error, const char* message);
void nl_util_init_atoms(ErlNifEnv* env);

#endif

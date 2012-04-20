#ifndef NODELUA_STATE
#define NODELUA_STATE

#include <erl_nif.h>

void* state_create(ErlNifEnv* env);
void state_destroy(ErlNifEnv* env);

int state_add_worker(ErlNifEnv* env);

ERL_NIF_TERM state_add_script(ErlNifEnv* env, const char * data, size_t size, const char * name);

#endif
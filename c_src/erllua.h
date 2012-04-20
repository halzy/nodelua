#ifndef ERL_LUA
#define ERL_LUA 1

#include <erl_nif.h>
#include <lua.h>

enum ERLLUA_STATE {
	ERLLUA_START,
	ERLLUA_YIELD,
	ERLLUA_DONE
};

typedef struct erllua_s* erllua_ptr;

void erllua_destroy(erllua_ptr erllua);
ERL_NIF_TERM erllua_create(ErlNifEnv* env, ErlNifResourceType* erllua_type, const char* data, const unsigned size, const char* name, erllua_ptr *erllua_result);
int erllua_run(erllua_ptr erllua);

#endif
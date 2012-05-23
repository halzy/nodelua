#ifndef ERL_LUA
#define ERL_LUA 1

#include <erl_nif.h>
#include <lua.h>

typedef enum ERLLUA_STATES {
	ERLLUA_INIT,  // internal to erllua, should never be seen externally
	ERLLUA_START, // script pre-compiled successfully
	ERLLUA_ERROR, // script failed to pre-compile, error will be sent when script is run
	ERLLUA_YIELD, // script has yielded
	ERLLUA_DONE   // script has returned
} ERLLUA_STATE;

typedef struct erllua* erllua_ptr;

void erllua_destroy(erllua_ptr erllua);
erllua_ptr erllua_create(ErlNifEnv* env, const char* data, const unsigned size, const char* name, const void* state_work, ErlNifResourceType* erl_resource_type);
ERLLUA_STATE erllua_run(erllua_ptr erllua);
int erllua_send_message(erllua_ptr erllua, ERL_NIF_TERM message);

int erllua_shutting_down(erllua_ptr erllua);

int erllua_refcount(erllua_ptr erllua);
int erllua_addref(erllua_ptr erllua);
int erllua_decref(erllua_ptr erllua);

#endif

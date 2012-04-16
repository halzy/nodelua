
#include "state.h"
#include "erllua.h"

#include <string.h>

typedef struct state
{
	ErlNifThreadOpts* thread_opts;
	ErlNifResourceType* erllua_type;
} state_t;

typedef struct state* state_ptr;

static state_ptr get_state(ErlNifEnv* env)
{
  return (state_ptr) enif_priv_data(env);
}

ERL_NIF_TERM state_add_script(ErlNifEnv* env, const char * data, size_t size, const char * name)
{
	ERL_NIF_TERM result;

	state_ptr state = get_state(env);

	erllua_ptr erllua;

	// returns NULL in erllua if there was an error and the error in result
	result = erllua_create(env, state->erllua_type, data, size, name, &erllua); 

	if(NULL != erllua)
	{
		// TODO: save erllua in a queue or something for processing
	}

	return result;
}


static void state_cleanup(ErlNifEnv* env, void* arg)
{
  // TODO @@@ remove from any sort of work queue

  // Delete any dynamically allocated memory stored in nodelua_handle
  erllua_ptr erllua = (erllua_ptr)arg;
  erllua_destroy(erllua);
}


void* state_create(ErlNifEnv* env)
{

    state_ptr state = (state_ptr) enif_alloc(sizeof(state_t));
    memset(state, '\0', sizeof(state_t));

	ErlNifResourceType* erllua_type = enif_open_resource_type(env, NULL, "nodelua_RESOURCE",
																&state_cleanup,
																ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

	state->erllua_type = erllua_type;
	if(NULL == state->erllua_type)
		goto error;

    state->thread_opts = enif_thread_opts_create("nodelua_worker");
    if(NULL == state->thread_opts) 
    	goto error;
    
    return state;

error:
	state_destroy(env);
	return NULL;
}


void state_destroy(ErlNifEnv* env)
{
	state_ptr state = get_state(env);
	
	if(NULL != state->thread_opts)
	{
		enif_thread_opts_destroy(state->thread_opts);
	}

	if(NULL != state)
	{
		enif_free(state);
		state = NULL;
	}
}

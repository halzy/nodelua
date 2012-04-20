
#include "state.h"
#include "erllua.h"
#include "queue.h"

#include <string.h>
#include <stdio.h>

struct state
{
	ErlNifThreadOpts* thread_opts;
	ErlNifResourceType* erllua_type;
	queue_ptr work_queue;

	ErlNifTid thread;
};

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
		queue_push(state->work_queue, erllua);
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

void destroy_work_queue(void* data)
{
	erllua_ptr erllua = (erllua_ptr) data;
	erllua_destroy(erllua);
}

void* state_create(ErlNifEnv* env)
{
    state_ptr state = (state_ptr) enif_alloc(sizeof(struct state));
    memset(state, '\0', sizeof(struct state));

	ErlNifResourceType* erllua_type = enif_open_resource_type(env, NULL, "nodelua_RESOURCE",
																&state_cleanup,
																ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

	state->work_queue = queue_create(&destroy_work_queue);
	if(NULL == state->work_queue)
		goto error;

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

	// destroy all the threads
	// TODO @@@
	void* resp;
	queue_push(state->work_queue, NULL);
    enif_thread_join(state->thread, &resp);

	if(NULL != state->work_queue)
	{
		queue_destroy(state->work_queue);
	}
	
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



static void* thr_main(void* state_ref)
{
    state_ptr state = (state_ptr) state_ref;
    ErlNifEnv* env = enif_alloc_env();

    erllua_ptr erllua = NULL;
    while(queue_pop(state->work_queue, (void**)&erllua))
    {
		if(NULL != erllua)
		{
			int lua_state = erllua_run(erllua);
			if(lua_state == ERLLUA_YIELD)
			{
				queue_push(state->work_queue, erllua);
			}
			enif_clear_env(env);
		}
    }

    return NULL;
}


int state_add_worker(ErlNifEnv* env)
{
	state_ptr state = get_state(env);

	printf("state_add_worker called with %p\n", state);

    return enif_thread_create("", &(state->thread), thr_main, state, state->thread_opts);
}


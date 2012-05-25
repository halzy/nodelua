#include "node_memory.h"

#include "state.h"
#include "erllua.h"
#include "queue.h"
#include "nl_util.h"

#include <string.h>
#include <stdio.h>

#include <assert.h>

typedef struct resource* resource_ptr;

typedef enum WORK_STATES 
{
	WORK_ENQUEUED,
	WORK_PROCESSING,
	WORK_REQUEUE,
	WORK_WAIT
} WORK_STATE;

struct state_work
{
	erllua_ptr erllua;

	ErlNifRWLock* rwlock;
	WORK_STATE run_state;
};

struct resource
{
	state_work_ptr work;
	RESOURCE_REF_TYPE ref_type;
};

struct state
{
	ErlNifThreadOpts* thread_opts;
	ErlNifResourceType* resource_type;
	queue_ptr work_queue;

	queue_ptr threads;
};

typedef struct state* state_ptr;

static int send_message_core( state_ptr state, state_work_ptr work, ERL_NIF_TERM message);

int state_work_addref(state_work_ptr work)
{
	return erllua_addref(work->erllua);
}
int state_work_decref(state_work_ptr work)
{
	return erllua_decref(work->erllua);
}

static state_ptr get_state(ErlNifEnv* env)
{
	return (state_ptr) enif_priv_data(env);
}

static void destroy_thread(void* thread)
{
	(void) thread; // unused
	assert("this should never be called");
}

static void destroy_work(void* data)
{
	state_work_ptr work = (state_work_ptr) data;

	if(NULL != work->erllua)
	{
		erllua_destroy(work->erllua);
	}

	if(NULL != work->rwlock)
	{
		enif_rwlock_destroy(work->rwlock);
	}

	memset(work, 0, sizeof(struct state_work));

	node_free(work);
}

static state_work_ptr create_work(ErlNifEnv* env, const char * data, size_t size, const char * name, ErlNifResourceType* resource_type)
{
	// allocate the actual work unit, this is pointed to by the resource
	// and we will know that all references have been removed from this
	// work unit when the resource destructor is called
	state_work_ptr work = (state_work_ptr) node_alloc(sizeof(struct state_work));
	if(NULL == work)
		goto error_create_work;

	memset(work, 0, sizeof(struct state_work));

	work->run_state = WORK_WAIT;

	work->rwlock = enif_rwlock_create("work unit lock");
	if(NULL == work->rwlock)
		goto error_create_work;

	// put a new erllua into it. If there is a script error we will find out
	// later when a thread processes the script
	work->erllua = erllua_create(env, data, size, name, (void*) work, resource_type); 
	if(NULL == work->erllua)
		goto error_create_work;

	return work;

error_create_work:
	if(NULL != work)
	{
		destroy_work(work);
	}

	return NULL;
}


ERL_NIF_TERM state_make_resource(ErlNifEnv* env, void** resourc, ErlNifResourceType* resource_type, state_work_ptr state_work, RESOURCE_REF_TYPE ref_type)
{
	resource_ptr rsrc = enif_alloc_resource(resource_type, sizeof(struct resource));
	if(NULL == rsrc)
		goto error_make_resource;

	// clear it out
	memset(rsrc, 0, sizeof(struct resource));
	rsrc->work = state_work;
	rsrc->ref_type = ref_type;

	ERL_NIF_TERM resource_handle = enif_make_resource(env, rsrc);

	// release our reference to the new erlang variable, when we return
	// from this stack to erlang it will be available for GC if the
	// reference is lost. This is how we can tell when the system is
	// done with a script.
	enif_release_resource(rsrc);

	// set the result
	*resourc = rsrc;
	return resource_handle;

error_make_resource:

	*resourc = NULL;
	return make_error_tuple(env, ATOM_MEMORY, "enif_alloc_resource() returned NULL");
}


ERL_NIF_TERM state_add_script(ErlNifEnv* env, const char * data, size_t size, const char * name)
{
	state_ptr state = get_state(env);

	ERL_NIF_TERM result;
	state_work_ptr work = create_work(env, data, size, name, state->resource_type);
	if(NULL == work)
	{
		result = make_error_tuple(env, ATOM_MEMORY, "error creating work unit");
		goto error_add_script;
	}

	// assocate the resource type that will be GC'd in erlang
	resource_ptr resource = NULL;
	result = state_make_resource(env, (void**)&resource, state->resource_type, work, STRONG_REF);
	if(NULL == resource)
	{
		goto error_add_script;
	}

	// increase the ref count for the work
	erllua_addref(resource->work->erllua);

	result = enif_make_tuple2(env, ATOM_OK, result);

	return result;

error_add_script:
	assert(NULL != resource);

	if(NULL != resource)
	{
		// if we failed to make the erllua, we need to free the 'work'
		// we do not need to free the 'resource' because it will be GC'd
		if(NULL != resource->work)
		{
			destroy_work(resource->work);
		}
	}

	// note that we are not cleaning up the resource here, it will
	// be GC'd and pushed through the resource's cleanup function

	return result;
}

// A CaS like operator for the work state, with an option to enqueue
static int enqueue_work_cas(WORK_STATE compare_state, WORK_STATE enqueue_state, WORK_STATE else_state, state_ptr state, state_work_ptr work)
{
	int result = 0;

	enif_rwlock_rwlock(work->rwlock);
	if(compare_state == work->run_state)
	{
		work->run_state = enqueue_state;
		result = 1;
	}
	else
	{
		work->run_state = else_state;
	}
	enif_rwlock_rwunlock(work->rwlock);	

	if(result)
	{
		queue_push(state->work_queue, work);
	}

	return result;
}

static void resource_gc(ErlNifEnv* env, void* arg)
{
	// !!! This function is not allowed to call any
	// !!! term making functions !!!

	// TODO @@@ notify the script that it will be destroyed
	// and that it has one last chance to run before it 
	// is shut down, perhaps send it a message?

	resource_ptr resource = (resource_ptr) arg;

	if(STRONG_REF == resource->ref_type)
	{
		// remove our reference to the work unit
		int destroy = erllua_decref(resource->work->erllua);

		if(destroy)
		{
			state_ptr state = get_state(env);
			enqueue_work_cas( WORK_WAIT, WORK_ENQUEUED, WORK_REQUEUE, state, resource->work);
		}		
	}

	memset(arg, 0, sizeof(struct resource));
}


void* state_create(ErlNifEnv* env)
{
    state_ptr state = (state_ptr) node_alloc(sizeof(struct state));
    memset(state, 0, sizeof(struct state));

	ErlNifResourceType* resource_type = enif_open_resource_type(env, NULL, "nodelua_RESOURCE",
																&resource_gc,
																ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

	state->work_queue = queue_create(&destroy_work);
	if(NULL == state->work_queue)
		goto error;

	state->threads = queue_create(&destroy_thread);
	if(NULL == state->threads)
		goto error;

	state->resource_type = resource_type;
	if(NULL == state->resource_type)
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
	if(NULL != state->threads)
	{
		queue_ptr threads = queue_create(&destroy_thread);
		ErlNifTid* thread;
		while(queue_pop_nowait(state->threads, (void**)&thread))
		{
			queue_push(state->work_queue, NULL);
			queue_push(threads, thread);
		}
		while(queue_pop_nowait(threads, (void**)&thread))
		{
		    enif_thread_join(*thread, NULL);
		    node_free(thread);
		}
		queue_destroy(state->threads);
	}

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
		node_free(state);
		state = NULL;
	}
}





static void* thread_main(void* state_ref)
{
    state_ptr state = (state_ptr) state_ref;

    state_work_ptr work = NULL;
    while(queue_pop(state->work_queue, (void**)&work))
    {
    	if(NULL == work)
    		break;

		enif_rwlock_rwlock(work->rwlock);
		work->run_state = WORK_PROCESSING;
		enif_rwlock_rwunlock(work->rwlock);

		int lua_state = erllua_run(work->erllua);

		// items in the work queue may be GC'd by erlang, this is
		// signaled by the ref_count becomming zero
		int destroy = erllua_shutting_down(work->erllua);

		if(destroy)
		{
			destroy_work(work);
		}
		else
		{
			if(ERLLUA_YIELD == lua_state)
			{
				enqueue_work_cas( WORK_REQUEUE, WORK_ENQUEUED, WORK_WAIT, state, work);
			}
			else
			{
				printf("error: lua state: %d\n", lua_state);
			}
		}
    }

    printf("goodbye!\n");
    enif_thread_exit(NULL);

    return NULL;
}


int state_add_worker(ErlNifEnv* env)
{
	state_ptr state = get_state(env);
	ErlNifTid* thread = node_alloc(sizeof(ErlNifTid));
    int result = enif_thread_create("", thread, thread_main, state, state->thread_opts);    
	queue_push(state->threads, thread);
    return result;
}

static int send_message_core(state_ptr state, state_work_ptr work, ERL_NIF_TERM message)
{
	int result = erllua_send_message(work->erllua, message);
	if(result)
	{
		enqueue_work_cas( WORK_WAIT, WORK_ENQUEUED, WORK_REQUEUE, state, work);
	}
	return result;
}

int state_send_message(ErlNifEnv* env, ERL_NIF_TERM resource_term, ERL_NIF_TERM message)
{
	state_ptr state = get_state(env);

	resource_ptr resource = NULL;

	int result = enif_get_resource(env, resource_term, state->resource_type, (void**)&resource);
	if(result)
	{
		result = send_message_core(state, resource->work, message);
	}

	return result;
}

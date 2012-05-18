
#include "state.h"
#include "erllua.h"
#include "queue.h"
#include "nl_util.h"

#include <string.h>
#include <stdio.h>

#include <assert.h>

typedef struct resource* resource_ptr;
typedef struct work* work_ptr;

typedef enum WORK_STATES 
{
	WORK_ENQUEUED,
	WORK_PROCESSING,
	WORK_REQUEUE,
	WORK_WAIT
} WORK_STATE;

struct work
{
	erllua_ptr erllua;
	unsigned ref_count;

	ErlNifRWLock* rwlock;
	WORK_STATE run_state;
};

struct resource
{
	work_ptr work;
};

struct state
{
	ErlNifThreadOpts* thread_opts;
	ErlNifResourceType* resource_type;
	queue_ptr work_queue;

	ErlNifTid thread;
};

typedef struct state* state_ptr;

static int send_message_core( state_ptr state, work_ptr work, ERL_NIF_TERM message);

static state_ptr get_state(ErlNifEnv* env)
{
	return (state_ptr) enif_priv_data(env);
}

static void destroy_work(void* data)
{
	work_ptr work = (work_ptr) data;
	
	enif_rwlock_rwlock(work->rwlock);
	unsigned ref_count = work->ref_count;
	enif_rwlock_rwunlock(work->rwlock);

	// destroy should never be called unless all references 
	// to the work have been released
	assert(0 == ref_count);
	if(0 != ref_count)
	{
		return;
	}

	if(NULL != work->erllua)
	{
		erllua_destroy(work->erllua);
	}

	if(NULL != work->rwlock)
	{
		enif_rwlock_destroy(work->rwlock);
	}

	enif_free(work);
}

static work_ptr create_work(ErlNifEnv* env, const char * data, size_t size, const char * name, ErlNifResourceType* resource_type)
{
	// allocate the actual work unit, this is pointed to by the resource
	// and we will know that all references have been removed from this
	// work unit when the resource destructor is called
	work_ptr work = (work_ptr) enif_alloc(sizeof(struct work));
	if(NULL == work)
		goto error_create_work;

	memset(work, 0, sizeof(struct work));

	work->run_state = WORK_WAIT;

	work->rwlock = enif_rwlock_create("work unit lock");
	if(NULL == work->rwlock)
		goto error_create_work;

	// put a new erllua into it. If there is a script error we will find out
	// later when a thread processes the script
	work->erllua = erllua_create(env, data, size, name, resource_type); 
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

ERL_NIF_TERM state_add_script(ErlNifEnv* env, const char * data, size_t size, const char * name)
{
	ERL_NIF_TERM result;

	state_ptr state = get_state(env);

	// assocate the resource type that will be GC'd in erlang
	resource_ptr resource = enif_alloc_resource(state->resource_type, sizeof(struct resource));
	if(NULL == resource)
	{
		result = make_error_tuple(env, ATOM_MEMORY, "enif_alloc_resource() returned NULL");
		goto error_add_script;
	}

	// clear it out
	memset(resource, 0, sizeof(struct resource));

	resource->work = create_work(env, data, size, name, state->resource_type);
	if(NULL == resource->work)
	{
		result = make_error_tuple(env, ATOM_MEMORY, "error creating work unit");
		goto error_add_script;
	}

	ERL_NIF_TERM resource_handle = enif_make_resource(env, resource);
	// release our reference to the new erlang variable, when we return
	// from this stack to erlang it will be available for GC if the
	// reference is lost. This is how we can tell when the system is
	// done with a script.
	enif_release_resource(resource);

	// increase the ref count for the work
	++(resource->work->ref_count);

	result = enif_make_tuple2(env, ATOM_OK, resource_handle);

	return result;

error_add_script:

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
static int enqueue_work_cas(WORK_STATE compare_state, WORK_STATE enqueue_state, WORK_STATE else_state, state_ptr state, work_ptr work)
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

	int destroy = 0;

	// remove our reference to the work unit
	enif_rwlock_rwlock(resource->work->rwlock);
	destroy = (0 == --(resource->work->ref_count));
	enif_rwlock_rwunlock(resource->work->rwlock);

	if(destroy)
	{
		state_ptr state = get_state(env);
		enqueue_work_cas( WORK_WAIT, WORK_ENQUEUED, WORK_REQUEUE, state, resource->work);
	}
}


void* state_create(ErlNifEnv* env)
{
    state_ptr state = (state_ptr) enif_alloc(sizeof(struct state));
    memset(state, 0, sizeof(struct state));

	ErlNifResourceType* resource_type = enif_open_resource_type(env, NULL, "nodelua_RESOURCE",
																&resource_gc,
																ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

	state->work_queue = queue_create(&destroy_work);
	if(NULL == state->work_queue)
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
	// TODO @@@
	void* resp = NULL;
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





static void* thread_main(void* state_ref)
{
    state_ptr state = (state_ptr) state_ref;

    work_ptr work = NULL;
    while(queue_pop(state->work_queue, (void**)&work))
    {
    	if(NULL == work)
    		break;

		enif_rwlock_rwlock(work->rwlock);
		work->run_state = WORK_PROCESSING;
		erllua_shut_down(work->erllua, (0 == work->ref_count));
		enif_rwlock_rwunlock(work->rwlock);

		int lua_state = erllua_run(work->erllua);

		// items in the work queue may be GC'd by erlang, this is
		// signaled by the ref_count becomming zero
		enif_rwlock_rlock(work->rwlock);
		int destroy = (0 == work->ref_count);
		erllua_shut_down(work->erllua, destroy);
		enif_rwlock_runlock(work->rwlock);

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

    enif_thread_exit(NULL);

    return NULL;
}


int state_add_worker(ErlNifEnv* env)
{
	state_ptr state = get_state(env);

    return enif_thread_create("", &(state->thread), thread_main, state, state->thread_opts);
}

static int send_message_core(state_ptr state, work_ptr work, ERL_NIF_TERM message)
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

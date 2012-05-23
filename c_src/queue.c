#include "node_memory.h"

#include "queue.h"
#include <string.h>
#include <assert.h>

typedef struct node* node_ptr;

enum INSERT_LOCATION { FRONT, END };

struct node
{
	void* data;
	node_ptr next;
};

struct queue
{
	ErlNifMutex*	lock;
	ErlNifCond*		cond;

	void (*destroy_node)(void*);

	node_ptr	first;
	node_ptr	last;
	unsigned	size;
};


// return a new queue or null if it couldn't be allocated
queue_ptr queue_create( void (*destroy_node)(void*) )
{
	queue_ptr queue = (queue_ptr) node_alloc(sizeof(struct queue));

	if(NULL == queue)
		goto error;

	memset(queue, 0, sizeof(struct queue));
	queue->destroy_node = destroy_node;
	
	queue->lock = enif_mutex_create("queue_lock");

	if(NULL == queue->lock)
		goto error;

	queue->cond = enif_cond_create("queue_condition");
	if(NULL == queue->cond)
		goto error;

	return queue;

error:

	if(NULL != queue)
	{
		queue_destroy(queue);
	}

	return NULL;
}

// do not call this if there is the possibility that items
// are still being added to the queue.
void queue_destroy(queue_ptr queue)
{
	assert(NULL != queue);

	// empty the queue, calling the destroy function for the items
	void* node = NULL;
	while(queue_pop_nowait(queue, &node))
	{
		if(NULL != queue->destroy_node)
		{
			queue->destroy_node(node);
		}
	}

	if(NULL != queue->lock)
	{
		enif_mutex_destroy(queue->lock);
	}

	if(NULL != queue->cond)
	{
		enif_cond_destroy(queue->cond);
	}

	memset(queue, 0, sizeof(struct queue));

	node_free(queue);
}

// returns 1 on success, 0 on failure
static int queue_insert(queue_ptr queue, void* data, int location)
{
	node_ptr node = (node_ptr) node_alloc(sizeof(struct node));

	if(NULL != node)
	{
		memset(node, 0, sizeof(struct node));

		node->data = data;

		enif_mutex_lock(queue->lock);
		if(0 == queue->size)
		{
			// queue is empty
			queue->first = node;
			queue->last = node;
		}
		else
		{
			if(FRONT == location)
			{
				node->next = queue->first;
				queue->first = node;
			}
			else
			{
				// append to queue
				queue->last->next = node;
				queue->last = node;
			}
		}
		++queue->size;

		enif_mutex_unlock(queue->lock);
		enif_cond_signal(queue->cond);
	}

	return NULL == node ? 0 : 1;
}

// returns 1 on success, 0 on failure
int queue_push(queue_ptr queue, void* data)
{
	return queue_insert(queue, data, END);
}

// returns 1 on success, 0 on failure
int queue_unpop(queue_ptr queue, void* data)
{
	return queue_insert(queue, data, FRONT);
}

static int queue_pop_core(queue_ptr queue, void **data, const int wait)
{
	node_ptr node = NULL;
	*data = NULL;

	enif_mutex_lock(queue->lock);

    while(1 == wait && 0 == queue->size)
    {
        enif_cond_wait(queue->cond, queue->lock);
    }

	if(queue->size > 0)
	{
		node = queue->first;

		// remove from queue, if we are shifting up the last one
		// then we are setting the 'first' one to NULL
		queue->first = node->next;

		--queue->size;
		if(0 == queue->size)
		{
			// queue is empty
			queue->last = NULL;
		}
	}	
	enif_mutex_unlock(queue->lock);

	// if we were able to get one, then we set the result and return
	if(NULL != node)
	{
		*data = node->data;
		node_free(node);
		return 1;
	}

	return 0;
}

int queue_pop(queue_ptr queue, void **data)
{
	return queue_pop_core(queue, data, 1);
}

int queue_pop_nowait(queue_ptr queue, void **data)
{
	return queue_pop_core(queue, data, 0);
}

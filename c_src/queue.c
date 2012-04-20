#include "queue.h"
#include <string.h>
#include <assert.h>

typedef struct node* node_ptr;

struct node
{
	void* data;
	node_ptr next;
};

struct queue
{
	ErlNifMutex*        lock;
	void (*destroy_node)(void*);

	node_ptr first;
	node_ptr last;
	unsigned size;
};


// return a new queue or null if it couldn't be allocated
queue_ptr queue_create( void (*destroy_node)(void*) )
{
	queue_ptr queue = (queue_ptr) enif_alloc(sizeof(struct queue));

	if(NULL != queue)
	{
		memset(queue, '\0', sizeof(struct queue));
		queue->lock = enif_mutex_create("queue_lock");

		if(NULL == queue->lock)
		{
			enif_free(queue);
			queue = NULL;
		}
		else
		{
			queue->destroy_node = destroy_node;
		}
	}

	return queue;
}

void queue_destroy(queue_ptr queue)
{
	assert(NULL != queue);

	if(NULL != queue->destroy_node)
	{
		void* node = NULL;
		while(queue_pop(queue, &node))
		{
			queue->destroy_node(node);
		}
	}

	// release the lock
	enif_mutex_destroy(queue->lock);

	// and the queue
	free(queue);
}

int queue_push(queue_ptr queue, void * data)
// returns 1 on success, 0 on failure
{
	node_ptr node = (node_ptr) enif_alloc(sizeof(struct node));

	if(NULL != node)
	{
		memset(node, '\0', sizeof(struct node));

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
			// append to queue
			queue->last->next = node;
			queue->last = node;
		}
		++queue->size;
		enif_mutex_unlock(queue->lock);
	}

	return NULL == node ? 0 : 1;
}

int queue_pop(queue_ptr queue, void **data)
{
	node_ptr node = NULL;
	*data = NULL;

	enif_mutex_lock(queue->lock);
	if(queue->size > 0)
	{
		node = queue->first;

		// remove from queue
		queue->first = node->next;

		--queue->size;
		if(0 == queue->size)
		{
			// queue is empty
			queue->last = NULL;
		}
	}	
	enif_mutex_unlock(queue->lock);

	if(NULL != node)
	{
		*data = node->data;
		enif_free(node);
		return 1;
	}

	return 0;
}

#ifndef NODELUE_QUEUE
#define NODELUE_QUEUE true

#include <erl_nif.h>

typedef struct queue* queue_ptr;

queue_ptr queue_create( void (*destroy_queue)(void*) );
void queue_destroy(queue_ptr queue);

// for inserting into the queue's front and end
int queue_push(queue_ptr queue, void* data);
int queue_unpop(queue_ptr queue, void* data);

// for retrieving from the queue, if the queue is empty
// queue_pop will wait for an item to be added to the 
// queue before returning
int queue_pop_nowait(queue_ptr queue, void** data);
int queue_pop(queue_ptr queue, void** data);

#endif

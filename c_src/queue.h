#ifndef NODELUE_QUEUE
#define NODELUE_QUEUE true

#include <erl_nif.h>

typedef struct queue* queue_ptr;

queue_ptr queue_create( void (*destroy_queue)(void*) );
void queue_destroy(queue_ptr queue);
int queue_push(queue_ptr queue, void * data);
int queue_pop(queue_ptr queue, void **data);
int queue_unpop(queue_ptr queue, void *data);

#endif

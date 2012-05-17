#include "message_queue.h"

#include "queue.h"
#include <stdio.h>

struct message_queue
{
  queue_ptr incoming;
  ErlNifEnv* env_incoming;

  queue_ptr processing;
  ErlNifEnv* env_processing;

  // cleared after every send
  ErlNifEnv* env_sending;

  ErlNifRWLock* swap_lock;
};


static ERL_NIF_TERM destroy_message(void* data)
{
  ERL_NIF_TERM message;
  ERL_NIF_TERM *message_copy = (ERL_NIF_TERM*)data;
  message = *message_copy;
  enif_free(data);
  return message; 
}

// a wrapper around destroy_message in order to pass it
// to the queue_create function
static void destroy_queued_message(void* data)
{
  destroy_message(data);
}

void destroy_message_queue(message_queue_ptr messages)
{
  if(NULL != messages->incoming)
    queue_destroy(messages->incoming);
  if(NULL != messages->processing)
    queue_destroy(messages->processing);
  if(NULL != messages->env_incoming)
    enif_free_env(messages->env_incoming);
  if(NULL != messages->env_processing)
    enif_free_env(messages->env_processing);
  if(NULL != messages->env_sending)
    enif_free_env(messages->env_sending);
  if(NULL != messages->swap_lock)
    enif_rwlock_destroy(messages->swap_lock);
  if(NULL != messages)
    enif_free(messages);
}

message_queue_ptr create_message_queue()
{
  message_queue_ptr message_queue = enif_alloc(sizeof(struct message_queue));
  if(NULL == message_queue)
	goto error_create_message_queue;

  message_queue->swap_lock = enif_rwlock_create("message queue lock");
  if(NULL == message_queue->swap_lock)
    goto error_create_message_queue;

  message_queue->incoming = queue_create(destroy_queued_message);
  if(NULL == message_queue->incoming)
    goto error_create_message_queue;

  message_queue->env_incoming = enif_alloc_env();
  if(NULL == message_queue->env_incoming)
    goto error_create_message_queue;

  message_queue->processing = queue_create(destroy_queued_message);
  if(NULL == message_queue->processing)
    goto error_create_message_queue;

  message_queue->env_processing = enif_alloc_env();
  if(NULL == message_queue->env_processing)
    goto error_create_message_queue;

  message_queue->env_sending = enif_alloc_env();
  if(NULL == message_queue->env_sending)
    goto error_create_message_queue;

  return message_queue;

error_create_message_queue:
  if(NULL != message_queue)
  {
    destroy_message_queue(message_queue);
  }
  return NULL;
}

void message_queue_process_begin(message_queue_ptr messages)
{
  enif_rwlock_rwlock(messages->swap_lock);
  queue_ptr temp = messages->incoming;
  messages->incoming = messages->processing;
  messages->processing = temp;

  ErlNifEnv* env = messages->env_incoming;
  messages->env_incoming = messages->env_processing;
  messages->env_processing = env; 
  enif_rwlock_rwunlock(messages->swap_lock);
}

int message_queue_push(message_queue_ptr messages, ERL_NIF_TERM message)
{
  ERL_NIF_TERM* message_copy = enif_alloc(sizeof(ERL_NIF_TERM));
  if(NULL == message_copy)
    return 0;

  enif_rwlock_rlock(messages->swap_lock);
  *message_copy = enif_make_copy(messages->env_incoming, message);
  int result = queue_push(messages->incoming, message_copy);
  enif_rwlock_runlock(messages->swap_lock);
  
  if(!result)
  {
    enif_free(message_copy);
  }
  return result;
}


ErlNifEnv* message_queue_process_getenv(message_queue_ptr messages)
{
  return messages->env_processing;  
}


ErlNifEnv* message_queue_sending_getenv(message_queue_ptr messages)
{
  return messages->env_sending;  
}


int message_queue_pop( message_queue_ptr messages, ERL_NIF_TERM *message)
{
  // no locking on swap_lock needed since pop-ing is 
  // in the thread that does the swap
  ERL_NIF_TERM *message_copy;
  int had_item = queue_pop_nowait(messages->processing, (void**)&message_copy);
  if(had_item)
  {
    *message = destroy_message(message_copy);
  }

  return had_item;
}



void message_queue_process_end(message_queue_ptr messages)
{
  ERL_NIF_TERM test;

  // using a read lock here so that we can also call _pop.
  // this is safe because of how it is used, we are modifying
  // the env but since the queue 'should' be empty and have
  // no readers since this same thread would have run the
  // lua instance which pop'd the messages. 
  enif_rwlock_rlock(messages->swap_lock);
  int had_item = message_queue_pop(messages, &test);
  if(!had_item)
  {
    enif_clear_env(messages->env_processing);
  }
  else
  {
    printf("ERROR: PROCESSING QUEUE WAS NOT EMPTY!\n");
  }
  enif_rwlock_runlock(messages->swap_lock);
}


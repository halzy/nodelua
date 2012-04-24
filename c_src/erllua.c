#include "erllua.h"
#include "nl_util.h"
#include "queue.h"

#include <erl_nif.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <string.h>

typedef struct message_queue* message_queue_ptr;
struct message_queue
{
  queue_ptr incoming;
  ErlNifEnv* env_incoming;

  queue_ptr processing;
  ErlNifEnv* env_processing;

  ErlNifRWLock* swap_lock;
};

struct erllua
{
  lua_State *lua;
  lua_State *coroutine;
  ERLLUA_STATE state;

  message_queue_ptr messages;
};

struct lua_input_script
{
  const char *data;
  const size_t size;
  int done;
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

static void destroy_message_queue(message_queue_ptr message_queue)
{
  if(NULL != message_queue->incoming)
    queue_destroy(message_queue->incoming);
  if(NULL != message_queue->processing)
    queue_destroy(message_queue->processing);
  if(NULL != message_queue->env_incoming)
    enif_free_env(message_queue->env_incoming);
  if(NULL != message_queue->env_processing)
    enif_free_env(message_queue->env_processing);
  if(NULL != message_queue->swap_lock)
    enif_rwlock_destroy(message_queue->swap_lock);
  if(NULL != message_queue)
    enif_free(message_queue);
}

static message_queue_ptr create_message_queue()
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

  return message_queue;

error_create_message_queue:
  if(NULL != message_queue)
  {
    destroy_message_queue(message_queue);
  }
  return NULL;
}

int erllua_send_message(erllua_ptr erllua, ERL_NIF_TERM message)
{
  ERL_NIF_TERM* message_copy = enif_alloc(sizeof(ERL_NIF_TERM));
  if(NULL == message_copy)
    return 0;

  enif_rwlock_rlock(erllua->messages->swap_lock);
  *message_copy = enif_make_copy(erllua->messages->env_incoming, message);
  int result = queue_push(erllua->messages->incoming, message_copy);
  enif_rwlock_runlock(erllua->messages->swap_lock);
  
  if(!result)
  {
    enif_free(message_copy);
  }
  return result;
}

static int erllua_message_processing_pop(erllua_ptr erllua, ERL_NIF_TERM *message)
{
  ERL_NIF_TERM *message_copy;
  enif_rwlock_rlock(erllua->messages->swap_lock);
  int had_item = queue_pop_nowait(erllua->messages->processing, (void**)&message_copy);
  enif_rwlock_runlock(erllua->messages->swap_lock);
  if(had_item)
  {
    *message = destroy_message(message_copy);
  }

  return had_item;
}

static void erllua_message_queue_swap(erllua_ptr erllua)
{
  enif_rwlock_rwlock(erllua->messages->swap_lock);
  queue_ptr temp = erllua->messages->incoming;
  erllua->messages->incoming = erllua->messages->processing;
  erllua->messages->processing = temp;

  ErlNifEnv* env = erllua->messages->env_incoming;
  erllua->messages->env_incoming = erllua->messages->env_processing;
  erllua->messages->env_processing = env; 
  enif_rwlock_rwunlock(erllua->messages->swap_lock);
}

static void erllua_message_processing_clear(erllua_ptr erllua)
{
  ERL_NIF_TERM test;
  enif_rwlock_rwlock(erllua->messages->swap_lock);
  int had_item = erllua_message_processing_pop(erllua, &test);
  if(!had_item)
  {
    enif_clear_env(erllua->messages->env_processing);
  }
  else
  {
    printf("ERROR: PROCESSING QUEUE WAS NOT EMPTY!\n");
  }
  enif_rwlock_rwunlock(erllua->messages->swap_lock);
}


void erllua_destroy(erllua_ptr erllua)
{
  lua_close(erllua->lua);
  destroy_message_queue(erllua->messages);
  enif_free(erllua);
}


ERLLUA_STATE erllua_run(erllua_ptr erllua)
{
  // TODO @@@ if state is error, send back the error on the top
  // of the stack
  /*
        result = make_error_tuple(env, ATOM_LUA, lua_tostring(coroutine, -1));
      lua_pop(coroutine, 1);
      lua_close(lua); 
*/
  // swap the message queues, make the incoming the processing one
  erllua_message_queue_swap(erllua);  
  
  // lua is expected to empty the message queue
  int result = lua_resume(erllua->coroutine, 0);

  // clear the environmest for the queue to let erlang GC some unused terms
  erllua_message_processing_clear(erllua);

  switch(result)
  {
    case 0:
    {
      erllua->state = ERLLUA_DONE;
      break;
    }
    case LUA_YIELD:
    {
      erllua->state = ERLLUA_YIELD;
      break;
    }
    default:
    {
      // error case, inspect stack
      erllua->state = ERLLUA_DONE;
      if(result == LUA_ERRRUN)
      {
        // TODO @@@ this should be sent to the parent process
        printf("Lua Error: %s\n", lua_tostring(erllua->coroutine, -1));
        lua_pop(erllua->coroutine, -1);
      }
      else
      {
        printf("Didn't handle case %d in erllua_run\n", erllua->state);
      }

      break;
    }
  }

  return erllua->state;
}

static const char *read_input_script(lua_State *env, void *user_data, size_t *size)
{
  struct lua_input_script *input = (struct lua_input_script *) user_data;

  // return NULL if we are done reading
  if(1 == input->done)
    return NULL;

  // give it all we've got and signal that we are done
  *size = input->size;
  input->done = 1;

  return input->data;
}

// returns {ok, handle} and sets erllua_result or {error, {kind, message}} and erllua_result is NULL
erllua_ptr erllua_create(ErlNifEnv* env, const char* data, const unsigned size, const char* name)
{
  erllua_ptr erllua = enif_alloc(sizeof(erllua));
  if(NULL == erllua)
    goto error_create;

  memset(erllua, 0, sizeof(erllua));
  erllua->state = ERLLUA_INIT;

  // create the message queue
  erllua->messages = create_message_queue();
  if(NULL == erllua->messages)
    goto error_create;

  // create a new lua state and only attach it to erlang on success
  erllua->lua = luaL_newstate();
  if(NULL == erllua->lua)
    goto error_create;

  erllua->coroutine = lua_newthread(erllua->lua);
  if(NULL == erllua->coroutine)
    goto error_create;

  luaL_openlibs(erllua->coroutine);
  struct lua_input_script input_script = { .data = data, .size = size, .done = 0 };

  // Load the command and try to execute it... we don't want to hold onto
  // the binary longer than this, so give it to lua and store it as lua
  // runtime script
  int load_result = lua_load(erllua->coroutine, read_input_script, &input_script, name);
  switch(load_result)
  {
    case 0:
    {
      erllua->state = ERLLUA_START;
      break;
    }
    case LUA_ERRSYNTAX:
    {
      erllua->state = ERLLUA_ERROR;
      break;
    }
    case LUA_ERRMEM:
    {
      goto error_create;
      break;
    }
  }

  return erllua;

error_create:

  if(NULL != erllua)
  {
    erllua_destroy(erllua);
  }

  return NULL;
}

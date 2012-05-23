#include "node_memory.h"

#include "erllua.h"
#include "nl_util.h"
#include "queue.h"
#include "message_queue.h"
#include "mailbox.h"

#include <erl_nif.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#include <string.h>



struct erllua
{
  lua_State *lua;
  lua_State *coroutine;
  ERLLUA_STATE state;

  message_queue_ptr messages;
  int do_shutdown;
};

struct lua_input_script
{
  const char *data;
  const size_t size;
  int done;
};

int erllua_shutting_down(erllua_ptr erllua)
{
  return erllua->do_shutdown;
}

void erllua_shut_down(erllua_ptr erllua, int shutdown)
{
  erllua->do_shutdown = shutdown;
}

int erllua_send_message(erllua_ptr erllua, ERL_NIF_TERM message)
{
  return message_queue_push(erllua->messages, message);
}


void erllua_destroy(erllua_ptr erllua)
{
  lua_close(erllua->lua);
  destroy_message_queue(erllua->messages);
  memset(erllua, 0, sizeof(struct erllua));
  node_free(erllua);
}


ERLLUA_STATE erllua_run(erllua_ptr erllua)
{
  // TODO @@@ if state is error, send back the error on the top
  // of the stack
  if(ERLLUA_ERROR == erllua->state)
  {
    //result = make_error_tuple(env, ATOM_LUA, lua_tostring(coroutine, -1));
    printf("LUA_ERROR: %s\n", lua_tostring(erllua->coroutine, -1));
    lua_pop(erllua->coroutine, 1);
    //lua_close(lua);
  }

  // swap the message queues, make the incoming the processing one
  message_queue_process_begin(erllua->messages);  
  // lua is expected to empty the message queue
  int result = lua_resume(erllua->coroutine, NULL, 0);

  // clear the environmest for the queue to let erlang GC some unused terms
  message_queue_process_end(erllua->messages);

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
  (void) env; // unused

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
erllua_ptr erllua_create(ErlNifEnv* env, const char* data, const unsigned size, const char* name, ErlNifResourceType* erl_resource_type)
{
  (void) env; // unused
  
  erllua_ptr erllua = node_alloc(sizeof(struct erllua));
  if(NULL == erllua)
    goto error_create;

  memset(erllua, 0, sizeof(struct erllua));
  erllua->state = ERLLUA_INIT;
  erllua->do_shutdown = 0;

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
  int load_result = lua_load(erllua->coroutine, read_input_script, &input_script, name, "bt");
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

  // add my libs here
  register_mailbox(erllua->coroutine, erllua, erllua->messages, erl_resource_type);

  return erllua;

error_create:

  if(NULL != erllua)
  {
    erllua_destroy(erllua);
  }

  return NULL;
}

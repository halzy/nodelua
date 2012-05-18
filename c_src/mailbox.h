#ifndef NODELUA_MAILBOX_H
#define NODELUA_MAILBOX_H TRUE

#include "erllua.h"
#include "message_queue.h"

#include <lua.h>
#include <erl_nif.h>

void register_mailbox(lua_State *lua, erllua_ptr erllua, message_queue_ptr message_queue, ErlNifResourceType* erl_resource_type);

#endif
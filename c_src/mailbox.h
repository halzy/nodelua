#ifndef NODELUA_MAILBOX_H
#define NODELUA_MAILBOX_H TRUE

#include <lua.h>

#include "message_queue.h"

void register_mailbox(lua_State *lua, message_queue_ptr messages);

#endif
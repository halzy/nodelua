#ifndef NODELUA_MAILBOX_H
#define NODELUA_MAILBOX_H TRUE

#include <lua.h>

#include "message_queue.h"

LUALIB_API int luaopen_mailbox(lua_State *lua);

#endif
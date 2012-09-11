/**
Copyright (c) 2012 Benjamin Halsted <bhalsted@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the"Software"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
**/

#ifndef ERL_LUA
#define ERL_LUA 1

#include <erl_nif.h>
#include <lua.h>

typedef enum ERLLUA_STATES {
	ERLLUA_INIT,  // internal to erllua, should never be seen externally
	ERLLUA_START, // script pre-compiled successfully
	ERLLUA_ERROR, // script failed to pre-compile, error will be sent when script is run
	ERLLUA_YIELD, // script has yielded
	ERLLUA_DONE   // script has returned
} ERLLUA_STATE;

typedef struct erllua* erllua_ptr;

void erllua_destroy(erllua_ptr erllua);
erllua_ptr erllua_create(ErlNifEnv* env, ErlNifPid owner_pid, const char* data, const unsigned size, const char* name, const void* state_work, ErlNifResourceType* erl_resource_type);
ERLLUA_STATE erllua_run(erllua_ptr erllua);
int erllua_send_message(erllua_ptr erllua, ERL_NIF_TERM message);

int erllua_shutting_down(erllua_ptr erllua);
ErlNifPid erllua_parent_pid(erllua_ptr erllua);

int erllua_refcount(erllua_ptr erllua);
int erllua_addref(erllua_ptr erllua);
int erllua_decref(erllua_ptr erllua);

#endif

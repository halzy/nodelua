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

#ifndef NODELUA_STATE
#define NODELUA_STATE

#include <erl_nif.h>

typedef enum RESOURCE_REF_TYPES
{
	STRONG_REF,
	WEAK_REF
} RESOURCE_REF_TYPE;

typedef struct state_work* state_work_ptr;

void* state_create(ErlNifEnv* env);
void state_destroy(ErlNifEnv* env);

int state_add_worker(ErlNifEnv* env);

ERL_NIF_TERM state_add_script(ErlNifEnv* env, ErlNifPid owner_pid, const char * data, size_t size, const char * name);
int state_send_message(ErlNifEnv* env, ERL_NIF_TERM resource_term, ERL_NIF_TERM message);

ERL_NIF_TERM state_make_resource(ErlNifEnv* env, void** resource, ErlNifResourceType* resource_type, state_work_ptr data, RESOURCE_REF_TYPE ref_type);
int state_work_addref(state_work_ptr state_work);
int state_work_decref(state_work_ptr state_work);

#endif
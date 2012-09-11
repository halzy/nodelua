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

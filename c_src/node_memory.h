#ifndef NODE_MEMORY
#define NODE_MEMORY 1

//#define NODE_DEBUG 1

#ifndef NODE_DEBUG
#include <erl_nif.h>
#define node_alloc(n)   (enif_alloc( (n) ))
#define node_free(ptr)  (enif_free( (ptr) ))
#else
#include <stdlib.h>
#define node_alloc(n)   (malloc( (n) ))
#define node_free(ptr)  (free( (ptr) ))
#endif

#endif
#ifndef NODELUA_MESSAGE_QUEUE
#define NODELUA_MESSAGE_QUEUE TRUE

#include <erl_nif.h>

typedef struct message_queue* message_queue_ptr;

message_queue_ptr create_message_queue();
void destroy_message_queue(message_queue_ptr messages);

void message_queue_process_begin(message_queue_ptr messages);
void message_queue_process_end(message_queue_ptr messages);

int message_queue_push(message_queue_ptr messages, ERL_NIF_TERM message);
int message_queue_pop( message_queue_ptr messages, ERL_NIF_TERM *message);

#endif

#ifndef STACK_H
#define STACK_H

#include <stdlib.h>

#include "debug.h"
#include "global.h"
#include "node.h"

struct Stack {
    size_t size; // The size of each segment's data array in bytes
    struct StackSegment* current_segment;
};

struct Stack*   stack_make      (size_t size);
uint8_t*        stack_alloc     (struct Stack* stack, size_t size);
BOOL            stack_is_empty  (struct Stack* stack);
uint8_t*        segment_get_data            (struct StackSegment* segment);
uint8_t*        segment_get_next_free_addr  (struct StackSegment* segment);

// Derivations

struct Stack*   node_stack_make     ();
struct Stack*   compost_stack_make  ();
struct Node*    node_stack_alloc    (struct Stack* stack);
void            compost_stack_add   (struct Stack* stack, struct Node* node);
void            node_stack_print    (int ind, struct Stack* stack);
void            compost_stack_print (int ind, struct Stack* stack);

// Utilities

struct Node*    compost_stack_pop   (struct Stack* stack);

#endif
#ifndef STACK_H
#define STACK_H

#include <stdlib.h>

#include "debug.h"
#include "global.h"
#include "node.h"

struct Stack {
    struct StackSegment* current_segment;
};

struct Stack*   stack_make              (size_t segment_size);
uint8_t*    stack_alloc
    (struct Stack* stack, size_t segment_size, size_t elem_size);
bool_t      stack_is_empty              (const struct Stack* stack);
uint8_t*    stack_pop                   (struct Stack* stack, size_t elem_size);
void        stack_unpop                 (struct Stack* stack, size_t elem_size);
uint8_t*    stack_peek                  (struct Stack* stack, size_t elem_size);
uint8_t*    segment_get_data            (const struct StackSegment* segment);
uint8_t*    segment_get_next_free_addr  (const struct StackSegment* segment);

// Derivations

struct Stack*   node_stack_make         ();
struct Stack*   freelist_stack_make     ();
struct Node*    node_stack_alloc        (struct Stack* stack);
void            freelist_stack_push     (struct Stack* stack,
    const struct Node* node);
struct Node*    freelist_stack_peek     (struct Stack* stack);
void            node_stack_print        (int ind, struct Stack* stack);
void            freelist_stack_print    (int ind, struct Stack* stack);

// Utilities

struct Node*        freelist_stack_pop       (struct Stack* stack);

#endif
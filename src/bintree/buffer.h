#ifndef BUFFER_H
#define BUFFER_H

#include <stdlib.h>

#include "debug.h"
#include "global.h"
#include "node.h"

struct Buffer {
    size_t capacity; // in bytes
    size_t size; // in bytes
    void* data;
};

struct Buffer   buffer_make     (size_t initial_size);
Index          buffer_push     (struct Buffer* buffer, size_t elem_size,
                                const void* data);
bool_t          buffer_is_empty (struct Buffer buffer);
void*           buffer_pop      (struct Buffer* buffer, size_t elem_size);
void            buffer_unpop    (struct Buffer* buffer, size_t elem_size);
void*           buffer_peek     (const struct Buffer* buffer, size_t elem_size);

// Derivations

struct Buffer   node_buffer_make    ();
Index           node_buffer_push    (struct Buffer* buffer, Index left,
                                    Index right);
void            node_buffer_print   (int ind, struct Buffer buffer);

#endif
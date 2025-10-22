#ifndef ARRAY_H
#define ARRAY_H

#include <stdlib.h>

#include "debug.h"
#include "global.h"
#include "node.h"

struct Array {
    size_t capacity; // Max amount of data in bytes the array can hold
    size_t size; // Currently held amount of data in bytes
    void* data; // uint8_t* implies that the data can be read at
    // byte-granularity, while void* does not
};

struct Array    array_make      (size_t initial_size);
void*           array_get       (struct Array array, Index index,
                                size_t elem_size);
void            array_set       (struct Array array, Index index,
                                size_t elem_size, const void* data);
Index           array_push      (struct Array* array, size_t elem_size,
                                const void* data);
bool_t          array_is_empty  (struct Array array);
void*           array_pop       (struct Array* array, size_t elem_size);
void*           array_peek      (struct Array* array, size_t elem_size);
void            array_unpop     (struct Array* array, size_t elem_size);

// Derivations

// Node vector
struct Array    node_array_make     ();
Index           node_array_push     (struct Array* array, Node node);
Node            node_array_get      (struct Array array, Index index);
void            node_array_set      (struct Array array, Index index,
                                    Node node);
size_t          node_array_count    (struct Array array);
// void            node_array_print    (int ind, struct Array array);

// Spine stack
struct Array    spine_array_make    ();
void            spine_array_push    (struct Array* array, Index node_index);
Index           spine_array_pop     (struct Array* array, bool_t* error);
Index           spine_array_peek    (struct Array* array, bool_t* error);
Index           spine_array_unpop   (struct Array* array);

#endif
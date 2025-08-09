#include "buffer.h"

#include <inttypes.h>
#include <string.h>

// ---------------------------------- DECLS ------------------------------------

void _grow_buffer(struct Buffer* buffer);

// ------------------------------ PUBLIC METHODS -------------------------------

struct Buffer buffer_make(size_t initial_size) {
    struct Buffer buffer;
    buffer.capacity = initial_size;
    buffer.size = 0;
    buffer.data = malloc(initial_size);

    return buffer;
}

// Allocate elem_size bytes, return its offset
Index buffer_push(struct Buffer* buffer, size_t elem_size, const void* data)
{
    if (buffer->capacity - buffer->size < elem_size) {
        _grow_buffer(&buffer);
    }

    Index result = buffer->size;
    memcpy((uint8_t*)buffer->data + buffer->size, data, elem_size);
    buffer->size += elem_size;

    return result;
}

bool_t buffer_is_empty(struct Buffer buffer) {
    return buffer.size == 0 ? TRUE : FALSE;
}

void* buffer_pop(struct Buffer* buffer, size_t elem_size) {
    if (buffer_is_empty(*buffer) == TRUE)
    {
        return NULL;
    }
    buffer->size -= elem_size;
    return (uint8_t*)buffer->data + buffer->size;
}

// NOTE: Unsafe, use with caution
void buffer_unpop(struct Buffer* buffer, size_t elem_size) {
    buffer->size += elem_size;
}

void* buffer_peek(const struct Buffer* buffer, size_t elem_size) {
    return (uint8_t*)buffer->data + buffer->size;
}

struct Buffer node_buffer_make() {
    return buffer_make(NODE_BUFFER_CAPACITY);
}

struct Node* node_buffer_push(struct Buffer* buffer, Index left, Index right) {
    struct Node node;
    node.left = left;
    node.right = right;
    return buffer_push(buffer, sizeof(struct Node), &node);
}

void node_buffer_print(int ind, struct Buffer* buffer) {
    debug_indent(ind, "\n----------\nNODES:\n----------\n");
    struct Node* cursor = (struct Node*)buffer->data;
    while (cursor <= (uint8_t*)buffer->data + buffer->size) {
        debug_indent(ind, "%" PRIuPTR ": %lu %lu\n", (uintptr_t)cursor,
            cursor->left, cursor->right);
        cursor++;
    }
}

// ---------------------------- INTERNAL METHODS -------------------------------

void _grow_buffer(struct Buffer* buffer) {
    void* ptr = realloc(buffer->data, buffer->capacity * GROWTH_FACTOR);
    if (ptr == NULL) {
        fail("realloc() failed: unable to grow buffer from %zu to %zu bytes\n",
            buffer->capacity, buffer->capacity * GROWTH_FACTOR);
    } else {
        buffer->capacity *= GROWTH_FACTOR;
        buffer->data = ptr;
    }
}


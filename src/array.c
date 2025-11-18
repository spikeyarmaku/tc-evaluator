#include "array.h"

#include <string.h>

const char node_type_table[] = {'L', 'S', 'F', 'A'};

// ---------------------------------- DECLS ------------------------------------

void _grow_array(struct Array* array);

// ------------------------------ PUBLIC METHODS -------------------------------

struct Array array_make(size_t initial_size) {
    struct Array array;
    array.capacity = initial_size;
    array.size = 0;
    array.data = malloc(initial_size);

    return array;
}

void* array_get(struct Array array, Index index, size_t elem_size) {
    return array.data + index * elem_size;
}

void array_set(struct Array array, Index index, size_t elem_size,
    const void* data)
{
    memcpy((uint8_t*)array.data + index * elem_size, data, elem_size);
}

// Allocate elem_size bytes, return its offset
Index array_push(struct Array* array, size_t elem_size, const void* data)
{
    if (array->capacity - array->size < elem_size) {
        _grow_array(array);
    }

    Index result = array->size / elem_size;
    array_set(*array, result, elem_size, data);
    // memcpy((uint8_t*)array->data + array->size, data, elem_size);
    array->size += elem_size;

    return result;
}

bool_t array_is_empty(struct Array array) {
    return array.size == 0 ? TRUE : FALSE;
}

void* array_pop(struct Array* array, size_t elem_size) {
    if (array_is_empty(*array) == TRUE) {
        return NULL;
    }
    array->size -= elem_size;
    return (uint8_t*)array->data + array->size;
}

void* array_peek(struct Array* array, size_t elem_size) {
    if (array_is_empty(*array) == TRUE) {
        return NULL;
    }
    return (uint8_t*)array->data + array->size - elem_size;
}

// NOTE: Unsafe, use with caution
void array_unpop(struct Array* array, size_t elem_size) {
    array->size += elem_size;
}

void array_free(struct Array* array) {
    free(array->data);
    array->capacity = 0;
    array->size = 0;
}

struct Array node_array_make(size_t capacity) {
    return array_make(capacity * sizeof(Node));
}

Index node_array_push(struct Array* array, Node node) {
    return array_push(array, sizeof(Node), &node);
}

void node_array_pop(struct Array* array) {
    array_pop(array, sizeof(Node));
}

Node node_array_get(struct Array array, Index index) {
    return ((Node*)array.data)[index];
}

Node* node_array_get_ref(struct Array array, Index index) {
    return (Node*)array.data + index;
}

void node_array_set(struct Array array, Index index, Node node) {
    array_set(array, index, sizeof(Node), &node);
}

size_t node_array_count(struct Array array) {
    return array.size / sizeof(Node);
}

struct Array spine_array_make(size_t capacity) {
    return array_make(capacity * sizeof(Index));
}

void spine_array_push(struct Array* array, Index node_index) {
    array_push(array, sizeof(Index), &node_index);
}

Index spine_array_pop(struct Array* array, bool_t* error) {
    Index* result = (Index*)array_pop(array, sizeof(Index));
    if (result == NULL) {
        *error = TRUE;
        return 0;
    }
    *error = FALSE;
    return *result;
}

Index spine_array_peek(struct Array* array, bool_t* error) {
    Index* result = (Index*)array_peek(array, sizeof(Index));
    if (result == NULL) {
        *error = TRUE;
        return 0;
    }
    *error = FALSE;
    return *result;
}

Index spine_array_unpop(struct Array* array) {
    array_unpop(array, sizeof(Index));
}

// ---------------------------- INTERNAL METHODS -------------------------------

void _grow_array(struct Array* array) {
    void* ptr = realloc(array->data, array->capacity * GROWTH_FACTOR);
    if (ptr == NULL) {
        fail("realloc() failed: unable to grow array from %zu to %zu bytes\n",
            array->capacity, array->capacity * GROWTH_FACTOR);
    } else {
        array->capacity *= GROWTH_FACTOR;
        array->data = ptr;
    }
}

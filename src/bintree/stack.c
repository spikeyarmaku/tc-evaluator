#include "stack.h"

// ----------------------------------- DECLS ----------------------------------
// When a segment is full, next_free_addr == terminator_addr
struct StackSegment {
    uint8_t* next_free_addr; // The address of the first free space
    uint8_t* terminator_addr; // The first out-of-bounds address
    uint8_t* data;
    struct StackSegment* next_segment;
    struct StackSegment* prev_segment;
};

struct StackSegment* _segment_make(size_t size);
void _segment_insert_new(struct Stack* stack);
void _stack_incr(struct Stack* stack, size_t size);
void _node_segment_print(int ind, struct StackSegment* segment);
void _compost_segment_print(int ind, struct StackSegment* segment);
struct StackSegment* _segment_copy(struct StackSegment* segment, size_t size);

// ------------------------------ PUBLIC METHODS ------------------------------
struct Stack* stack_make(size_t size) {
    struct Stack* stack = malloc(sizeof(struct Stack));
    stack->size = size;
    stack->current_segment = _segment_make(size);

    return stack;
}

uint8_t* stack_alloc(struct Stack* stack, size_t size) {
    uint8_t* result = stack->current_segment->next_free_addr;
    _stack_incr(stack, size);
    return result;
}

BOOL stack_is_empty(struct Stack* stack) {
    if (stack->current_segment->prev_segment == NULL &&
        stack->current_segment->next_free_addr == stack->current_segment->data)
    {
        return TRUE;
    }
    return FALSE;
}

uint8_t* segment_get_data(struct StackSegment* segment) {
    return segment->data;
}

uint8_t* segment_get_next_free_addr(struct StackSegment* segment) {
    return segment->next_free_addr;
}

struct Stack* node_stack_make() {
    return stack_make(NODE_SEGMENT_SIZE * sizeof(struct Node));
}

struct Stack* compost_stack_make() {
    return stack_make(COMPOST_SEGMENT_SIZE * sizeof(struct Node*));
}

struct Node* node_stack_alloc(struct Stack* stack) {
    return (struct Node*)stack_alloc(stack, sizeof(struct Node));
}

void compost_stack_add(struct Stack* stack, struct Node* node) {
    assert(node != NULL);
    struct Node** slot = (struct Node**)stack_alloc(stack,
        sizeof(struct Node*));
    *slot = node;
}

void node_stack_print(int ind, struct Stack* stack) {
    struct StackSegment* curr_segment = stack->current_segment;
    while (curr_segment->prev_segment != NULL) {
        curr_segment = curr_segment->prev_segment;
    }
    
    int segment_count = 0;
    do {
        debug_indent(ind, "\n");
        debug_indent(ind, "N-SEGMENT #%2d:\n", segment_count);
        debug_indent(ind, "----------\n");
        _node_segment_print(ind, curr_segment);
        curr_segment = curr_segment->next_segment;
        segment_count++;
    } while (curr_segment != NULL);
}

void compost_stack_print(int ind, struct Stack* stack) {
    struct StackSegment* curr_segment = stack->current_segment;
    while (curr_segment->prev_segment != NULL) {
        curr_segment = curr_segment->prev_segment;
    }

    int segment_count = 0;
    do {
        debug_indent(ind, "\n");
        debug_indent(ind, "C-SEGMENT #%2d:\n", segment_count);
        debug_indent(ind, "----------\n");
        _compost_segment_print(ind, curr_segment);
        curr_segment = curr_segment->next_segment;
        segment_count++;
    } while (curr_segment != NULL);
}

struct Node* compost_stack_pop(struct Stack* stack) {
    // If there's no entry in the current segment
    if (stack->current_segment->next_free_addr == stack->current_segment->data)
    {
        // If this is the first segment
        if (stack->current_segment->prev_segment == NULL) {
            return NULL;
        } else {
            stack->current_segment = stack->current_segment->prev_segment;
        }
    } else {
        stack->current_segment->next_free_addr -= sizeof(struct Node*);
    }
    return *(struct Node**)stack->current_segment->next_free_addr;
}

// ----------------------------- INTERNAL METHODS -----------------------------

struct StackSegment* _segment_make(size_t size) {
    struct StackSegment* segment = malloc(sizeof(struct StackSegment));
    segment->data = malloc(size);
    segment->next_free_addr = segment->data;
    segment->terminator_addr = segment->data + size;
    segment->next_segment = NULL;
    segment->prev_segment = NULL;
    return segment;
}

void _segment_insert_new(struct Stack* stack) {
    struct StackSegment* new_segment = _segment_make(stack->size);
    stack->current_segment->next_segment = new_segment;
    new_segment->prev_segment = stack->current_segment;
}

void _stack_incr(struct Stack* stack, size_t size) {
    if (stack->current_segment->next_free_addr ==
        stack->current_segment->terminator_addr)
    {
        if (stack->current_segment->next_segment == NULL) {
            _segment_insert_new(stack);
        }
        stack->current_segment = stack->current_segment->next_segment;
    } else {
        stack->current_segment->next_free_addr += size;
    }
}

void _node_segment_print(int ind, struct StackSegment* segment) {
    struct Node* node = (struct Node*)segment->data;

    int node_counter = 0;
    while ((uint8_t*)node != segment->next_free_addr) {
        debug_indent(ind, "Node #%4d Addr. %14lu: %14lu %14lu\n", node_counter,
            (size_t)node, (size_t)node->left, (size_t)node->right);
        node_counter++;
        node++;
    }
}

void _compost_segment_print(int ind, struct StackSegment* segment) {
    struct Node** node = (struct Node**)segment->data;

    int node_counter = 0;
    while ((uint8_t*)node != segment->next_free_addr) {
        debug_indent(ind, "Node #%4d Addr. %14lu: %14lu\n", node_counter,
            (size_t)node, (size_t)*node);
        node_counter++;
        node++;
    }
}

struct StackSegment* _segment_copy(struct StackSegment* segment, size_t size) {
    struct StackSegment* result = malloc(sizeof(struct StackSegment));
    result->data = malloc(size);
    result->next_free_addr = segment->next_free_addr;
    result->terminator_addr = segment->terminator_addr;
    result->next_segment = NULL;
    result->prev_segment = NULL;
    return result;
}


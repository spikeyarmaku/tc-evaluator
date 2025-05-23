#include "stack.h"

#include <inttypes.h>

// ----------------------------------- DECLS ----------------------------------
// TODO Make `data` a uniform-sized array inside the struct
// When a segment is full, next_free_addr == terminator_addr
struct StackSegment {
    uint8_t* next_free_addr; // The address of the first free space
    uint8_t* terminator_addr; // The first out-of-bounds address
    struct StackSegment* next_segment;
    struct StackSegment* prev_segment;
    uint8_t data[];
};

static struct StackSegment* _segment_make(size_t size);
static void _segment_insert_new(struct Stack* stack, size_t size);
static void _node_segment_print(int ind, struct StackSegment* segment);
static void _freelist_segment_print(int ind, struct StackSegment* segment);

// ------------------------------ PUBLIC METHODS ------------------------------
struct Stack* stack_make(size_t segment_size) {
    struct Stack* stack = malloc(sizeof(struct Stack));
    stack->current_segment = _segment_make(segment_size);

    return stack;
}

uint8_t* stack_alloc(struct Stack* stack, size_t segment_size, size_t elem_size)
{
    assert(elem_size <= segment_size);

    uint8_t* result = stack->current_segment->next_free_addr;
    uint8_t* new_next = stack->current_segment->next_free_addr + elem_size;
    if (new_next >= stack->current_segment->terminator_addr) {
        if (stack->current_segment->next_segment == NULL) {
            _segment_insert_new(stack, segment_size);
        }
        stack->current_segment = stack->current_segment->next_segment;
        result = stack->current_segment->next_free_addr;
        new_next = stack->current_segment->next_free_addr + elem_size;
    }

    stack->current_segment->next_free_addr = new_next;
    return result;
}

bool_t stack_is_empty(const struct Stack* stack) {
    if (stack->current_segment->prev_segment == NULL &&
        stack->current_segment->next_free_addr == stack->current_segment->data)
    {
        return TRUE;
    }
    return FALSE;
}

uint8_t* segment_get_data(const struct StackSegment* segment) {
    return (uint8_t*)segment->data;
}

uint8_t* segment_get_next_free_addr(const struct StackSegment* segment) {
    return segment->next_free_addr;
}

uint8_t* stack_pop(struct Stack* stack, size_t elem_size) {
    // If there's no entry in the current segment
    if (stack->current_segment->next_free_addr == stack->current_segment->data)
    {
        // If this is the first segment
        if (stack->current_segment->prev_segment == NULL) {
            return NULL;
        } else {
            stack->current_segment = stack->current_segment->prev_segment;
        }
    }
    stack->current_segment->next_free_addr -= elem_size;
    return stack->current_segment->next_free_addr;
}

void stack_unpop(struct Stack* stack, size_t elem_size) {
    uint8_t* new_next = stack->current_segment->next_free_addr + elem_size;
    if (new_next >= stack->current_segment->terminator_addr) {
        stack->current_segment = stack->current_segment->next_segment;
        new_next = stack->current_segment->next_free_addr + elem_size;
    }

    stack->current_segment->next_free_addr = new_next;
}

uint8_t* stack_peek(struct Stack* stack, size_t elem_size) {
    struct StackSegment* temp = stack->current_segment;
    // If there's no entry in the current segment
    if (temp->next_free_addr == temp->data)
    {
        // If this is the first segment
        if (temp->prev_segment == NULL) {
            return NULL;
        } else {
            temp = temp->prev_segment;
        }
    }
    return temp->next_free_addr - elem_size;
}

struct Stack* node_stack_make() {
    return stack_make(NODE_SEGMENT_SIZE);
}

struct Stack* freelist_stack_make() {
    return stack_make(FREELIST_SEGMENT_SIZE);
}

struct Node* node_stack_alloc(struct Stack* stack) {
    return (struct Node*)stack_alloc(stack, NODE_SEGMENT_SIZE,
        sizeof(struct Node));
}

void freelist_stack_push(struct Stack* stack, const struct Node* node) {
    assert(node != NULL);
    struct Node** slot = (struct Node**)stack_alloc(stack,
        FREELIST_SEGMENT_SIZE, sizeof(struct Node*));
    *slot = (struct Node*)node;
}

struct Node* freelist_stack_peek(struct Stack* stack) {
    return *(struct Node**)stack_peek(stack, sizeof(struct Node*));
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

void freelist_stack_print(int ind, struct Stack* stack) {
    struct StackSegment* curr_segment = stack->current_segment;
    while (curr_segment->prev_segment != NULL) {
        curr_segment = curr_segment->prev_segment;
    }

    int segment_count = 0;
    do {
        debug_indent(ind, "\n");
        debug_indent(ind, "C-SEGMENT #%2d:\n", segment_count);
        debug_indent(ind, "----------\n");
        _freelist_segment_print(ind, curr_segment);
        curr_segment = curr_segment->next_segment;
        segment_count++;
    } while (curr_segment != NULL);
}

struct Node* freelist_stack_pop(struct Stack* stack) {
    return (struct Node*)stack_pop(stack, sizeof(struct Node*));
}

// ----------------------------- INTERNAL METHODS -----------------------------

static struct StackSegment* _segment_make(size_t size) {
    struct StackSegment* segment = malloc(sizeof(struct StackSegment) + size);
    segment->next_free_addr = segment->data;
    segment->terminator_addr = segment->data + size;
    segment->next_segment = NULL;
    segment->prev_segment = NULL;
    return segment;
}

static void _segment_insert_new(struct Stack* stack,size_t size) {
    struct StackSegment* new_segment = _segment_make(size);
    stack->current_segment->next_segment = new_segment;
    new_segment->prev_segment = stack->current_segment;
}

static void _node_segment_print(int ind, struct StackSegment* segment) {
    struct Node* node = (struct Node*)segment->data;

    int node_counter = 0;
    while ((uint8_t*)node != segment->next_free_addr) {
        debug_indent(ind, "Node #%4d Addr. %14" PRIuPTR ": %14lu %14lu\n",
            node_counter, (uintptr_t)node, node->left, node->right);
        node_counter++;
        node++;
    }
}

static void _freelist_segment_print(int ind, struct StackSegment* segment) {
    struct Node** node = (struct Node**)segment->data;

    int node_counter = 0;
    while ((uint8_t*)node != segment->next_free_addr) {
        debug_indent(ind, "Node #%4d Addr. %14" PRIuPTR ": %14lu\n",
            node_counter, (uintptr_t)node, (uintptr_t)*node);
        node_counter++;
        node++;
    }
}



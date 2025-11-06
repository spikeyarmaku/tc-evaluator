// Node structure:

// Bits
// 0 1 | 2 3 4 ... 17 18 | 19 20 21 ... 40 41 | 42 43 44 ... 63 64
// |     |                  |                   |
// |     |                  |                   +--- 23 bits: address of
// |     |                  |                        second child
// |     |                  +--- 23 bits: address of first child
// |     +--- 16 bits: Refcount
// +--- 2 bits: TAG (0 - special, 1 - stem, 2 - fork, 3 - app)

// An example for a stem node whose child is located at index 5:
// 1 0 | 1 0 0 0 ... 0 0 | 1 0 1 0 0 0 0 0 ... 0 0 | 0 0 0 ... 0
// |     |                 |
// |     |                 +--- address: 5 (0b1010000...)
// |     +--- refcount: 1 (0b1)
// +--- tag: 1 - stem (0b1)

// Example:
// t(t(tttt))
// A(A(A(FLL)L)L)L
// A-1-0 A-2-0 A-3-0 F-0-0

// A L a            -> S a
// A S a   c        -> F a c
// A F L   c u      -> c
// A F Sa  c u      -> AAacAbc
// A F Fab c L      -> a
// A F Fab c Su     -> Abu
// A F Fab c Fuv    -> AAcuv

// sample eval: AAAFLLLLL -> FLL
// (XXX means empty cell, [...] is the currently evaluated node)
// memory                   matched pattern
//[A10] A20  A30  F00       A A
// A10 [A20] A30  F00       A A
// A10  A20 [A30] F00       A F L c u         --- match!
// A10  A00  XXX  XXX       [reduction]
// A10 [A00] XXX  XXX       A L a
// A10 [S00] XXX  XXX       [reduction]
//[A10] S00  XXX  XXX       A S a c
//[F00] XXX  XXX  XXX       [reduction]
// F00  XXX  XXX  XXX       [end]

#include "node.h"

#include <inttypes.h>
#include <stdio.h>

#define TAG_MASK_SHIFT 0
#define REFCOUNT_MASK_SHIFT 2
#define LEFT_CHILD_MASK_SHIFT 18
#define RIGHT_CHILD_MASK_SHIFT 41

#define TAG_MASK (((uint_least64_t)1<<2) - 1)
#define REFCOUNT_MASK ((((uint_least64_t)1<<16) - 1) << REFCOUNT_MASK_SHIFT)
#define LEFT_CHILD_MASK ((((uint_least64_t)1<<23) - 1) << LEFT_CHILD_MASK_SHIFT)
#define RIGHT_CHILD_MASK \
    ((((uint_least64_t)1<<23) - 1) << RIGHT_CHILD_MASK_SHIFT)

// ------------------------------ PUBLIC METHODS ------------------------------

Node node_make(enum NodeTag tag, size_t refcount, Index left, Index right) {
    return tag + (refcount << REFCOUNT_MASK_SHIFT) +
        (left << LEFT_CHILD_MASK_SHIFT) + (right << RIGHT_CHILD_MASK_SHIFT);
}

Index node_get_indir(Node node) {
    return node_get_left_child_index(node);
}

void node_set_indir(Node* node, Index index) {
    node_set_tag(node, Indirection);
    node_set_left_child_index(node, index);
}

bool_t node_is_empty(Node node) {
    return node_get_refcount(node) == 0 ? TRUE : FALSE;
}

uint32_t node_get_refcount(Node node) {
    return get_masked(node, REFCOUNT_MASK) >> REFCOUNT_MASK_SHIFT;
}

void node_set_refcount(Node* node, uint32_t refcount) {
    *node = set_masked(*node, (uint_least64_t)refcount << REFCOUNT_MASK_SHIFT,
        REFCOUNT_MASK);
}

void node_incr_refcount(Node* node) {
    if (node_get_refcount(*node) == -1) {
        fail("PANIC! node_incr_refcount: Refcount overflow\n");
    }
    *node += (1 << REFCOUNT_MASK_SHIFT);
}

void node_decr_refcount(Node* node) {
    if (node_get_refcount(*node) != 0) {
        *node -= (1 << REFCOUNT_MASK_SHIFT);
    }
}

enum NodeTag node_get_tag(Node node) {
    return (enum NodeTag)(get_masked(node, TAG_MASK) >> TAG_MASK_SHIFT);
}

void node_set_tag(Node* node, enum NodeTag tag) {
    *node = set_masked(*node, (uint_least64_t)tag << TAG_MASK_SHIFT, TAG_MASK);
}

Index node_get_left_child_index(Node node) {
    return get_masked(node, LEFT_CHILD_MASK) >> LEFT_CHILD_MASK_SHIFT;
}

Index node_get_right_child_index(Node node) {
    return get_masked(node, RIGHT_CHILD_MASK) >> RIGHT_CHILD_MASK_SHIFT;
}

void node_set_left_child_index(Node* node, Index left) {
    *node = set_masked(*node, left << LEFT_CHILD_MASK_SHIFT, LEFT_CHILD_MASK);
}

void node_set_right_child_index(Node* node, Index right) {
    *node = set_masked(*node, right << RIGHT_CHILD_MASK_SHIFT,
        RIGHT_CHILD_MASK);
}

int node_print(char* buffer, Node node) {
    int char_count = 0;
    char* tags[] = {"I", "S", "F", "A"};
    enum NodeTag tag = node_get_tag(node);
    if (tag == Indirection) {
        char_count = sprintf(buffer, "%s[%u].%lu", tags[tag],
            node_get_refcount(node), node_get_indir(node));
    } else {
        char_count = sprintf(buffer, "%s[%u].%lu.%lu", tags[tag],
            node_get_refcount(node), node_get_left_child_index(node),
            node_get_right_child_index(node));
    }
    buffer[char_count] = 0;
    return char_count;
}

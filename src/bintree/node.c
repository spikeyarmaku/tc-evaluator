// Node structure:

// Bits
// 0 1 | 2 3 4 ... 31 32 | 33 34 35 ... 79 80 | 81 82 83 ... 126 127
// |     |                  |                   |
// |     |                  |                   +--- 48 bits: address of
// |     |                  |                        second child
// |     |                  +--- 48 bits: address of first child
// |     +--- 30 bits: Refcount
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
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define TAG_MASK_SHIFT 0
#define REFCOUNT_MASK_SHIFT 2
#define LEFT_CHILD_MASK_SHIFT 32
#define RIGHT_CHILD_MASK_SHIFT 80

#define TAG_MASK (((uint_least64_t)1<<2) - 1)
#define REFCOUNT_MASK (REFCOUNT_LIMIT << REFCOUNT_MASK_SHIFT)
#define LEFT_CHILD_MASK (CHILD_LIMIT << LEFT_CHILD_MASK_SHIFT)
#define RIGHT_CHILD_MASK (CHILD_LIMIT << RIGHT_CHILD_MASK_SHIFT)

// ------------------------------ PUBLIC METHODS ------------------------------

Node node_make(enum NodeTag tag, size_t refcount, Index left, Index right) {
    Node result;
    uint_least32_t tag_refcount = tag + (refcount << REFCOUNT_MASK_SHIFT);
    memcpy(result.data, &tag_refcount, 4);
    node_set_left_child_index(&result, left);
    node_set_right_child_index(&result, right);
    return result;
}

Node node_make_empty() {
    Node result = {0, 0, 0, 0, 0, 0};
    return result;
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
    uint_least64_t refcount;
    memcpy(&refcount, node.data, 4);
    return get_masked(refcount, REFCOUNT_MASK) >> REFCOUNT_MASK_SHIFT;
}

void node_set_refcount(Node* node, uint32_t refcount) {
    uint_least64_t tag_refcount;
    memcpy(&tag_refcount, node->data, 4);
    tag_refcount = set_masked(tag_refcount,
        (uint_least64_t)refcount << REFCOUNT_MASK_SHIFT, REFCOUNT_MASK);
    memcpy(node->data, &tag_refcount, 4);
}

void node_incr_refcount(Node* node) {
    uint_least64_t refcount = node_get_refcount(*node);
    if (refcount == REFCOUNT_LIMIT) {
        fail("PANIC! node_incr_refcount: Refcount overflow\n");
    }
    node_set_refcount(node, refcount + 1);
}

void node_decr_refcount(Node* node) {
    uint_least64_t refcount = node_get_refcount(*node);
    if (refcount != 0) {
        node_set_refcount(node, refcount - 1);
    }
}

enum NodeTag node_get_tag(Node node) {
    return (enum NodeTag)(get_masked(node.data[0], TAG_MASK) >> TAG_MASK_SHIFT);
}

void node_set_tag(Node* node, enum NodeTag tag) {
    node->data[0] = set_masked(node->data[0],
        (uint_least64_t)tag << TAG_MASK_SHIFT, TAG_MASK);
}

Index node_get_left_child_index(Node node) {
    Index result = 0;
    memcpy(&result, node.data + 2, 6);
    return result;
}

Index node_get_right_child_index(Node node) {
    Index result = 0;
    memcpy(&result, node.data + 5, 6);
    return result;
}

void node_set_left_child_index(Node* node, Index left) {
    memcpy(node->data + 2, &left, 6);
}

void node_set_right_child_index(Node* node, Index right) {
    memcpy(node->data + 5, &right, 6);
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

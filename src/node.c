// Node structure:

// Bits
// 0 1 | 2 3 ... 12 13 | 14 15 ... 62 63 | 64 65 ... 78 79 | 80 81 ... 126 127
// |     |               |                 |                 |
// |     |               |                 |                 +-- 48 bits: addr.
// |     |               |                 |                     of second child
// |     |               |                 +--- 16 bits: refcount (last 16 bits)
// |     |               |
// |     |               +--- 48 bits: address of first child
// |     +--- 14 bits: Refcount (first 14 bits)
// +--- 2 bits: TAG (0 - special, 1 - stem, 2 - fork, 3 - app)

// An example for a stem node whose child is located at index 5:
// 1 0 | 1 0 0 0 ... 0 0 | 1 0 1 0 0 0 ... 0 0 | 0 0 0 ... 0 | 0 0 0 ... 0
// |     |                 |
// |     |                 +--- address: 5 (0b1010000...)
// |     +--- refcount: 1 (0b1)
// +--- tag: 1 - stem (0b1)

#include "node.h"
#include "global.h"

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define TAG_MASK (((uint_least64_t)1<<2) - 1)

// ------------------------------ PUBLIC METHODS ------------------------------

Node node_make(enum NodeTag tag, size_t refcount, Index left, Index right) {
    Node result;
    result.left =
        tag + (get_masked(refcount, ((size_t)1 << 14) - 1) << 2) + (left << 16);
    result.right = (refcount >> 14) + (right << 16);
    return result;
}

Node node_make_empty() {
    Node result;
    result.left = 0;
    result.right = 0;
    return result;
}

bool_t node_is_equal(Node node0, Node node1) {
    return
        (node0.left == node1.left && node0.right == node1.right) ? TRUE : FALSE;
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
    return get_masked(node.left >> 2, ((size_t)1 << 14) - 1) +
        (get_masked(node.right, ((size_t)1 << 16) - 1) << 16);
}

void node_set_refcount(Node* node, uint32_t refcount) {
    node->left =
        set_masked(node->left, refcount << 2, (((size_t)1 << 14) - 1) << 2);
    node->right =
        set_masked(node->right, refcount >> 14, ((size_t)1 << 16) - 1);
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
    return get_masked(node.left, TAG_MASK);
}

void node_set_tag(Node* node, enum NodeTag tag) {
    node->left = set_masked(node->left, tag, TAG_MASK);
}

Index node_get_left_child_index(Node node) {
    return node.left >> 16;
}

Index node_get_right_child_index(Node node) {
    return node.right >> 16;
}

void node_set_left_child_index(Node* node, Index left) {
    node->left =
        set_masked(node->left, left << 16, (((size_t)1 << 48) - 1) << 16);
}

void node_set_right_child_index(Node* node, Index right) {
    node->right =
        set_masked(node->right, right << 16, (((size_t)1 << 48) - 1) << 16);
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

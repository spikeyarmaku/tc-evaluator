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

Node node_make(enum NodeType type, size_t refcount, Index left, Index right) {
    Node result;
    if (type == NODE_TYPE_INDIR) {
        type = NODE_TYPE_STEM;
        if (left == 0) {
            left = 1;
        }
    }
    result.left = type + (get_masked(refcount, ((size_t)1 << 14) - 1) << 2) +
        (left << 16);
    result.right = (refcount >> 14) + (right << 16);
    return result;
}

Node node_make_empty() {
    return node_make(NODE_TYPE_APP, 0, 0, 0);
}

bool_t node_is_equal(Node node0, Node node1) {
    return
        (node0.left == node1.left && node0.right == node1.right) ? TRUE : FALSE;
}

Index node_get_indir(Node node) {
    return node_get_child(node, CHILD_SINGLE);
}

// Set type to NODE_TYPE_INDIR and set child index to the referenced index
Node node_set_indir(Node node, Index index) {
    return node_set_child(
        node_set_type(node, NODE_TYPE_INDIR), CHILD_SINGLE, index);
}

bool_t node_is_empty(Node node) {
    return node_get_refcount(node) == 0 ? TRUE : FALSE;
}

uint32_t node_get_refcount(Node node) {
    return get_masked(node.left >> 2, ((size_t)1 << 14) - 1) +
        (get_masked(node.right, ((size_t)1 << 16) - 1) << 16);
}

Node node_set_refcount(Node node, uint32_t refcount) {
    node.left =
        set_masked(node.left, refcount << 2, (((size_t)1 << 14) - 1) << 2);
    node.right =
        set_masked(node.right, refcount >> 14, ((size_t)1 << 16) - 1);
    return node;
}

Node node_incr_refcount(Node node) {
    uint_least64_t refcount = node_get_refcount(node);
    if (refcount == REFCOUNT_LIMIT) {
        fail("PANIC! node_incr_refcount: Refcount overflow\n");
    }
    return node_set_refcount(node, refcount + 1);
}

Node node_decr_refcount(Node node) {
    uint_least64_t refcount = node_get_refcount(node);
    if (refcount != 0) {
        return node_set_refcount(node, refcount - 1);
    }
    fail("PANIC! node_decr_refcount: Refcount underflow\n");
    return node;
}

enum NodeType node_get_type(Node node) {
    enum NodeType type = get_masked(node.left, TAG_MASK);
    if (type == NODE_TYPE_STEM && node_get_child(node, CHILD_SIDE_LEFT) != 0) {
        return NODE_TYPE_INDIR;
    }
    return type;
}

Node node_set_type(Node node, enum NodeType type) {
    if (type == NODE_TYPE_INDIR) {
        node = node_set_child(node, CHILD_SIDE_LEFT, 1);
        type = NODE_TYPE_STEM;
    }
    node.left = set_masked(node.left, type, TAG_MASK);
    return node;
}

Index node_get_child(Node node, enum ChildSide side) {
    if (side == CHILD_SIDE_LEFT) {
        return node.left >> 16;
    } else {
        return node.right >> 16;
    }
}

Node node_set_child(Node node, enum ChildSide side, Index index) {
    if (side == CHILD_SIDE_LEFT) {
        node.left =
            set_masked(node.left, index << 16, (((size_t)1 << 48) - 1) << 16);
    } else {
        node.right =
            set_masked(node.right, index << 16, (((size_t)1 << 48) - 1) << 16);
    }
    return node;
}

int node_print(char* buffer, Node node) {
    int char_count = 0;
    char* types[] = {"C", "S", "F", "A", "I"};
    enum NodeType type = node_get_type(node);
    char_count = sprintf(buffer, "%s[%u].%lu.%lu", types[type],
        node_get_refcount(node), node_get_child(node,
            CHILD_SIDE_LEFT), node_get_child(node, CHILD_SIDE_RIGHT));
    buffer[char_count] = 0;
    return char_count;
}

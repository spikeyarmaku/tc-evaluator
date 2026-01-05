#ifndef NODE_H
#define NODE_H

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "global.h"

#include "../include/shrubble.h"

// The tree is stored as a series of Nodes. A node is a 128-bit integer value:
// 2 bits for tag (Custom, Stem, Fork or Application), and if it isn't a custom
// node, 30 bits for refcount and 48-48 bits for child indices. Leaf nodes are
// not stored, as they don't have any children, and they contain no useful
// information - all leaf nodes are the same. They are represented by the 0
// index.
// Indirection nodes are useful when the parent node's child index has to be
// changed (e.g. with rules 1 and 3a) - they are essentially Stem nodes with a
// non-zero left child.

// See node.c for details and examples

typedef struct NodeData {
    uint64_t left;
    uint64_t right;
} Node;

// Create
Node            node_make           (enum NodeType type, size_t refcount,
    Index left, Index right);
Node            node_make_empty     ();

// Compare
bool_t          node_is_equal       (Node node0, Node node1);
bool_t          node_is_empty       (Node node);

// Get
Index           node_get_indir      (Node node);
enum NodeType   node_get_type       (Node node);
uint32_t        node_get_refcount   (Node node);
Index           node_get_child      (Node node, enum ChildSide side);

// Set
Node            node_set_indir      (Node node, Index index);
Node            node_set_type       (Node node, enum NodeType type);
Node            node_set_refcount   (Node node, uint32_t refcount);
Node            node_set_child      (Node node, enum ChildSide side,
    Index index);

Node            node_incr_refcount  (Node node);
Node            node_decr_refcount  (Node node);

int             node_print          (char* buffer, Node node);

#endif

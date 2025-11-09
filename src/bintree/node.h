#ifndef NODE_H
#define NODE_H

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "global.h"
#include "debug.h"

// The tree is stored as a series of Nodes. A node is a 128-bit integer value:
// 2 bits for tag (Indirection, Stem, Fork or Application), 30 bits for refcount
// and 48-48 bits for child indices. Leaf nodes are not stored, as they don't
// have any children, and they contain no useful information - all leaf nodes
// are the same. They are represented by the 0 index. Indirection nodes are
// useful when the parent node's child index has to be changed (e.g. with rules
// 1 and 3a).

// Further node type ideas (these, together with Indirection, can be special
// node types, and the Indirection tag could be renamed to Special):
// - Number
//      They contain a 32-bit natural number, that can be incrementally
//      unpacked into a tree
// - Compressed
//      They are trees that are represented as bits, where every two bits is a
//      node (0 - leaf, 1 - stem, 2 - fork, 3- app). They can also be
//      incrementally unpacked into a tree, but it could be quite slow.
//      Recommended for low-memory environments

// See node.c for details and examples

typedef struct NodeData {
    uint64_t left;
    uint64_t right;
} Node;

enum NodeTag {
    Indirection = 0,
    Stem        = 1,
    Fork        = 2,
    App         = 3
};

Node            node_make                   (enum NodeTag tag, size_t refcount,
    Index left, Index right);
Node            node_make_empty             ();
bool_t          node_is_equal               (Node node0, Node node1);
Index           node_get_indir              (Node node);
void            node_set_indir              (Node* node, Index index);
bool_t          node_is_empty               (Node node);
uint32_t        node_get_refcount           (Node node);
void            node_set_refcount           (Node* node, uint32_t refcount);
void            node_incr_refcount          (Node* node);
void            node_decr_refcount          (Node* node);
enum NodeTag    node_get_tag                (Node node);
enum SpecialNodeTag node_get_special_tag    (Node node);
void            node_set_tag                (Node* node, enum NodeTag tag);
Index           node_get_left_child_index   (Node node);
Index           node_get_right_child_index  (Node node);
void            node_set_left_child_index   (Node* node, Index left);
void            node_set_right_child_index  (Node* node, Index right);

int             node_print                  (char* buffer, Node node);

#endif

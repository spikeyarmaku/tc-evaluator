#ifndef TREE_H
#define TREE_H

#include <stdlib.h>
#include <assert.h>

#include "global.h"
#include "node.h"
#include "array.h"

#include <string.h>

struct Tree {
    // Nodes of the tree
    struct Array nodes;
    size_t free_space_count;
    Index search_start; // Entries before this are 100% non-empty
};

struct Tree tree_make               ();
Index       tree_add_node           (struct Tree* tree, enum NodeTag tag,
    Index left_child_index, Index right_child_index);
Node        tree_get_node           (struct Tree tree, Index index);
void        tree_set_node           (struct Tree* tree, Index index, Node node);
void        tree_incr_refcount      (struct Tree* tree, Index index);
void        tree_decr_refcount      (struct Tree* tree, Index index);
void        tree_delete_children    (struct Tree* tree, Index index);
Index       tree_search_free_spaces (struct Tree tree, size_t size,
    bool_t* error);

size_t      tree_get_node_count     (struct Tree tree);
void        tree_print              (struct Tree tree);
void        tree_debug_print        (struct Tree tree);

#endif
#ifndef TREE_H
#define TREE_H

#include <stdlib.h>
#include <assert.h>

#include "global.h"
#include "node.h"
#include "array.h"

#include <string.h>

struct Tree {
    struct Array nodes; // Nodes of the tree
    size_t free_space_count;
};

struct Tree tree_make               (size_t capacity);
void        tree_free               (struct Tree* tree);
Index       tree_add_node           (struct Tree* tree, Node node);
Node        tree_get_node           (struct Tree tree, Index index);
void        tree_set_node           (struct Tree tree, Index index, Node node);
void        tree_incr_refcount      (struct Tree* tree, Index index);
void        tree_decr_refcount      (struct Tree* tree, Index index);
void        tree_delete_node        (struct Tree* tree, Index index);
void        tree_delete_children    (struct Tree* tree, Index index);

size_t      tree_get_node_count     (struct Tree tree);
void        tree_print              (struct Tree tree, char* buffer,
    bool_t use_spaces);
void        tree_debug_print        (struct Tree tree);
void        tree_print_comb         (struct Tree tree);

bool_t      tree_check_free_spaces  (struct Tree tree,
    size_t* free_space_count);

#endif

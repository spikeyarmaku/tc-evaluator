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
Index       tree_add_node           (struct Tree* tree, enum NodeTag tag);
Node        tree_get_node           (struct Tree tree, Index index);
Node*       tree_get_node_ref       (struct Tree tree, Index index);
void        tree_copy_child         (struct Tree* tree, Index from_index,
    enum ChildSide from_side, Index to_index, enum ChildSide to_side);
void        tree_change_child       (struct Tree* tree, Index index,
    enum ChildSide side, Index new_index);
void        tree_detach_child       (struct Tree* tree, Index index,
    enum ChildSide side);
void        tree_change_tag         (struct Tree* tree, Index index,
    enum NodeTag tag);

size_t      tree_get_node_count     (struct Tree tree);
void        tree_print              (struct Tree tree, char* buffer,
    bool_t use_spaces);
void        tree_debug_print        (struct Tree tree);
void        tree_print_comb         (struct Tree tree);

bool_t      tree_check_free_spaces  (struct Tree tree,
    size_t* free_space_count);

#endif

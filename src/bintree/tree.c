#include <stdio.h>

#include "tree.h"

static void _tree_clear_node(struct Tree* tree, Index index);
void _tree_print_subtree(struct Tree tree, Index index, bool_t root);
size_t _tree_get_node_count(struct Tree tree, Index index, bool_t root);

// -----------------------------------------------------------------------------

struct Tree tree_make() {
    struct Tree tree;
    tree.nodes = node_array_make();
    tree.free_space_count = 0;
    tree.search_start = -1;
    return tree;
}

// Either allocate or reuse space for a new node, and fill it with data
Index tree_add_node(struct Tree* tree, enum NodeTag tag, Index left_child_index,
    Index right_child_index)
{
    bool_t no_free_space;
    Index new_node_index = tree_search_free_spaces(*tree, 1, &no_free_space);

    Node new_node = node_make(tag, 1, left_child_index, right_child_index);
    if (no_free_space == TRUE) {
        debug("Pushing node\n");
        new_node_index = node_array_push(&tree->nodes, new_node);
    } else {
        tree_delete_children(tree, new_node_index);
        tree_set_node(tree, new_node_index, new_node);
        tree->free_space_count--;
        size_t node_count = node_array_count(tree->nodes);
        if (tree->free_space_count == 0) {
            // Set the "first free node index" to the theoretical max, so if a
            // node is freed up, it is guaranteed to have a lower index, and
            // thus `search_start` gets updated properly
            tree->search_start = (size_t)-1;
        } else {
            tree->search_start = new_node_index + 1;
        }
    }

    return new_node_index;
}

Node tree_get_node(struct Tree tree, Index index) {
    return node_array_get(tree.nodes, index);
}

void tree_set_node(struct Tree* tree, Index index, Node node) {
    Node old_node = tree_get_node(*tree, index);
    node_array_set(tree->nodes, index, node);
}

void tree_incr_refcount(struct Tree* tree, Index index) {
    if (index == 0) {
        return;
    }

    Node node = tree_get_node(*tree, index);
    if (node_get_refcount(node) == 0) {
        tree->free_space_count--;
    }
    node_incr_refcount(&node);
    tree_set_node(tree, index, node);
}

void tree_decr_refcount(struct Tree* tree, Index index) {
    if (index == 0) {
        return;
    }
    Node node = tree_get_node(*tree, index);
    if (node_get_refcount(node) == 0) {
        return;
    }
    node_decr_refcount(&node);
    if (node_get_refcount(node) == 0) {
        tree_set_node(tree, index, node);
        if (tree->search_start > index) {
            tree->search_start = index;
        }
        debug("tree_decr_refcount %lu - Incr free space from %lu ", index,
            tree->free_space_count);
        tree->free_space_count++;
        debug("to %lu\n", tree->free_space_count);
    }
}

void tree_delete_children(struct Tree* tree, Index index) {
    Node node = tree_get_node(*tree, index);
    Index left = node_get_left_child_index(node);
    Index right = node_get_right_child_index(node);
    tree_decr_refcount(tree, left);
    tree_decr_refcount(tree, right);
}

// Start searching for `size` contiguous empty nodes from tree->search_start,
// and return the index of the first empty space
Index tree_search_free_spaces(struct Tree tree, size_t size, bool_t* error) {
    if (tree.free_space_count < size) {
        *error = TRUE;
        return 0;
    }

    Index search = tree.search_start;
    size_t elem_count = node_array_count(tree.nodes);
    size_t counter = 0;
    while ((size_t)search < elem_count) {
        if (node_is_empty(node_array_get(tree.nodes, search)) == TRUE) {
            counter++;
        } else {
            counter = 0;
        }
        search++;
        if (counter == size) {
            *error = FALSE;
            return search - size;
        }
    }
    *error = TRUE;
    return 0;
}

size_t tree_get_node_count(struct Tree tree) {
    return _tree_get_node_count(tree, 0, TRUE);
}

void tree_print(struct Tree tree) {
    if (node_array_count(tree.nodes) > 0) {
        _tree_print_subtree(tree, 0, TRUE);
        printf("\n");
    } else {
        printf("t\n");
    }
}

void _tree_print_subtree(struct Tree tree, Index index, bool_t root) {
    if (root == FALSE && index == 0) {
        printf("t");
        return;
    }
    Node top = tree_get_node(tree, index);
    switch (node_get_tag(top)) {
        case Indirection: {
            _tree_print_subtree(tree, node_get_indir(top), FALSE);
            break;
        }
        case Stem: {
            printf("t");
            Index left = node_get_left_child_index(top);
            if (left != 0) {
                printf("(");
            }
            _tree_print_subtree(tree, left, FALSE);
            if (left != 0) {
                printf(")");
            }
            break;
        }
        case Fork: {
            printf("t");
            Index left = node_get_left_child_index(top);
            if (left != 0) {
                printf("(");
            }
            _tree_print_subtree(tree, left, FALSE);
            if (left != 0) {
                printf(")");
            }

            Index right = node_get_right_child_index(top);
            if (right != 0) {
                printf("(");
            }
            _tree_print_subtree(tree, right, FALSE);
            if (right != 0) {
                printf(")");
            }
            break;
        }
        case App: {
            Index left = node_get_left_child_index(top);
            // if (left != 0) {
            //     printf("(");
            // }
            _tree_print_subtree(tree, left, FALSE);
            // if (left != 0) {
            //     printf(")");
            // }

            Index right = node_get_right_child_index(top);
            if (right != 0) {
                printf("(");
            }
            _tree_print_subtree(tree, right, FALSE);
            if (right != 0) {
                printf(")");
            }
            break;
        }
        default: {
            fail("PANIC! tree_print: invalid tag - %d\n", node_get_tag(top));
            break;
        }
    }
}

void tree_debug_print(struct Tree tree) {
    // Index cursor = 0;
    // size_t count = node_array_count(tree.nodes);
    // char buf[1024];
    // printf("Tree (%lu free, start at %lu):\n", tree.free_space_count,
    //     tree.search_start);
    // while (cursor < count) {
    //     node_print(buf, node_array_get(tree.nodes, cursor));
    //     printf("%lu: %s\n", cursor, buf);
    //     cursor++;
    // }
}

// ----------------------------- INTERNAL METHODS -----------------------------

static void _tree_clear_node(struct Tree* tree, Index index) {
    if (index < tree->search_start) {
        tree->search_start = index;
    }
    node_array_set(tree->nodes, index, 0);
}

size_t _tree_get_node_count(struct Tree tree, Index index, bool_t root) {
    if (root == FALSE && index == 0) {
        return 1;
    }
    Node node = tree_get_node(tree, index);
    switch (node_get_tag(node)) {
        case Indirection: {
            return _tree_get_node_count(tree, node_get_indir(node), FALSE);
            break;
        }
        case Stem: {
            return 1 +
                _tree_get_node_count(tree, node_get_left_child_index(node),
                FALSE);
            break;
        }
        case Fork: {
            return 1 +
                _tree_get_node_count(tree, node_get_left_child_index(node),
                FALSE) +
                _tree_get_node_count(tree, node_get_right_child_index(node),
                FALSE);
            break;
        }
        case App: {
            return
                _tree_get_node_count(tree, node_get_left_child_index(node),
                FALSE) +
                _tree_get_node_count(tree, node_get_right_child_index(node),
                FALSE);
            break;
        }
        default: {
            fail("PANIC! tree_get_node_count: invalid tag - %d\n",
                node_get_tag(node));
            break;
        }
    }
}

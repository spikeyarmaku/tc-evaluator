#include <stdio.h>

#include "tree.h"
#include "array.h"
#include "global.h"
#include "node.h"

static void _tree_mark_empty(struct Tree* tree, Index index);
static void _tree_delete_children(struct Tree* tree, Index index);
static void _tree_print_subtree(struct Tree tree, char* buffer,
    bool_t use_spaces, Index index, bool_t root);
static void _tree_print_comb_subtree(struct Tree tree, Index index,
    bool_t root);
static size_t _tree_get_node_count(struct Tree tree, Index index, bool_t root);

// -----------------------------------------------------------------------------

struct Tree tree_make(size_t capacity) {
    struct Tree tree;
    tree.nodes = node_array_make(capacity);
    tree.free_space_count = 0;
    return tree;
}

void tree_free(struct Tree* tree) {
    array_free(&tree->nodes);
    tree->free_space_count = 0;
}

Index tree_add_node(struct Tree* tree, Node node) {
    // Increase refcounts for the children
    Index new_node_index = 0;

    // Check if the last node is a marker for empty space
    if (tree->free_space_count != 0) {
        Index last_index = node_array_count(tree->nodes) - 1;
        assert(last_index != 0);
        Node last_node = tree_get_node(*tree, last_index);
        assert(node_get_refcount(last_node) == 0);
        new_node_index = node_get_indir(last_node);
        node_array_pop(&tree->nodes);
    }

    if (new_node_index != 0) {
        _tree_delete_children(tree, new_node_index);
        node_array_set(tree->nodes, new_node_index, node);
        tree->free_space_count--;
    } else {
        new_node_index = node_array_push(&tree->nodes, node);
    }

    if (new_node_index >= CHILD_LIMIT) {
        tree_debug_print(*tree);
        fail("Panic! tree_add_node: node index overflow\n");
    }
    return new_node_index;
}

Node tree_get_node(struct Tree tree, Index index) {
    return node_array_get(tree.nodes, index);
}

void tree_set_node(struct Tree tree, Index index, Node node) {
    node_array_set(tree.nodes, index, node);
}

size_t tree_get_node_count(struct Tree tree) {
    return node_array_count(tree.nodes);
}

void tree_print(struct Tree tree, char* buffer, bool_t use_spaces) {
    if (node_array_count(tree.nodes) - tree.free_space_count > 0) {
#ifdef USE_SPACES
        bool_t use_spaces = TRUE;
#else
        bool_t use_spaces = FALSE;
#endif
        buffer[0] = 0;
        _tree_print_subtree(tree, buffer, use_spaces, 0, TRUE);
    } else {
        sprintf(buffer, "t");
    }
}

void tree_debug_print(struct Tree tree) {
    Index cursor = 0;
    size_t count = node_array_count(tree.nodes);
    char buf[1024];
    printf("Tree (%lu free):\n", tree.free_space_count);
    while (cursor < count) {
        node_print(buf, node_array_get(tree.nodes, cursor));
        printf("%lu: %s\n", cursor, buf);
        cursor++;
    }
}

void tree_print_comb(struct Tree tree) {
if (node_array_count(tree.nodes) > 0) {
        _tree_print_comb_subtree(tree, 0, TRUE);
        printf("\n");
    } else {
        printf("L\n");
    }
}

// For debug purposes: checks if the reported amount of free spaces in a tree is
// correct
bool_t tree_check_free_spaces(struct Tree tree, size_t* free_space_count) {
    *free_space_count = 0;
    Index cursor = 0;
    size_t count = node_array_count(tree.nodes);
    while (cursor < count) {
        if (node_get_refcount(node_array_get(tree.nodes, cursor)) == 0) {
            (*free_space_count)++;
            if (*free_space_count > tree.free_space_count) {
                return FALSE;
            }
        }
        cursor++;
    }
    if (*free_space_count != tree.free_space_count) {
        return FALSE;
    }
    return TRUE;
}

void tree_incr_refcount(struct Tree* tree, Index index) {
    if (index == 0) {
        return;
    }

    Node node = tree_get_node(*tree, index);
    if (node_get_refcount(node) == 0 && node_get_type(node) == NODE_TYPE_INDIR)
    {
        tree->free_space_count--;
    }
    node = node_incr_refcount(node);
    node_array_set(tree->nodes, index, node);
}

void tree_decr_refcount(struct Tree* tree, Index index) {
    if (index == 0) {
        return;
    }
    Node node = tree_get_node(*tree, index);
    if (node_get_refcount(node) == 0) {
        return;
    }
    node = node_decr_refcount(node);
    node_array_set(tree->nodes, index, node);
    if (node_get_refcount(node) == 0) {
        _tree_mark_empty(tree, index);
    }
}

void tree_delete_node(struct Tree* tree, Index index) {
    tree_set_node(*tree, index, node_make_empty());
    _tree_mark_empty(tree, index);
}

// ----------------------------- INTERNAL METHODS -----------------------------

static void _tree_mark_empty(struct Tree* tree, Index index) {
    tree->free_space_count++;
    node_array_push(&tree->nodes, node_make(NODE_TYPE_INDIR, 0, 0, index));
}

static void _tree_delete_children(struct Tree* tree, Index index) {
    Node node = tree_get_node(*tree, index);
    switch (node_get_type(node)) {
        case NODE_TYPE_STEM: {
            tree_decr_refcount(tree, node_get_child(node, CHILD_SINGLE));
            break;
        }
        case NODE_TYPE_FORK:
        case NODE_TYPE_APP: {
            tree_decr_refcount(tree, node_get_child(node, CHILD_SIDE_LEFT));
            tree_decr_refcount(tree, node_get_child(node, CHILD_SIDE_RIGHT));
            break;
        }
        default: {
            break;
        }
    }
}

static void _tree_print_subtree(struct Tree tree, char* buffer,
    bool_t use_spaces, Index index, bool_t root)
{
    if (root == FALSE && index == 0) {
        strcat(buffer, "t");
        return;
    }
    Node top = tree_get_node(tree, index);
    switch (node_get_type(top)) {
        case NODE_TYPE_INDIR: {
            _tree_print_subtree(tree, buffer, use_spaces, node_get_indir(top),
                FALSE);
            break;
        }
        case NODE_TYPE_STEM: {
            strcat(buffer, "t");
            Index left = node_get_child(top, CHILD_SINGLE);
            if (use_spaces == TRUE) {
                strcat(buffer, " ");
            }
            if (left != 0) {
                strcat(buffer, "(");
            }
            _tree_print_subtree(tree, buffer, use_spaces, left, FALSE);
            if (left != 0) {
                strcat(buffer, ")");
            }
            break;
        }
        case NODE_TYPE_FORK: {
            strcat(buffer, "t");
            Index left = node_get_child(top, CHILD_SIDE_LEFT);
            if (use_spaces == TRUE) {
                strcat(buffer, " ");
            }
            if (left != 0) {
                strcat(buffer, "(");
            }
            _tree_print_subtree(tree, buffer, use_spaces, left, FALSE);
            if (left != 0) {
                strcat(buffer, ")");
            }
            Index right = node_get_child(top, CHILD_SIDE_RIGHT);
            if (use_spaces == TRUE) {
                strcat(buffer, " ");
            }
            if (right != 0) {
                strcat(buffer, "(");
            }
            _tree_print_subtree(tree, buffer, use_spaces, right, FALSE);
            if (right != 0) {
                strcat(buffer, ")");
            }
            break;
        }
        case NODE_TYPE_APP: {
            Index left = node_get_child(top, CHILD_SIDE_LEFT);
            _tree_print_subtree(tree, buffer, use_spaces, left, FALSE);
            Index right = node_get_child(top, CHILD_SIDE_RIGHT);
            if (use_spaces == TRUE) {
                strcat(buffer, " ");
            }
            if (right != 0) {
                strcat(buffer, "(");
            }
            _tree_print_subtree(tree, buffer, use_spaces, right, FALSE);
            if (right != 0) {
                strcat(buffer, ")");
            }
            break;
        }
        default: {
            fail("PANIC! tree_print: invalid tag - %d\n", node_get_type(top));
            break;
        }
    }
}

static void _tree_print_comb_subtree(struct Tree tree, Index index, bool_t root)
{
    if (root == FALSE && index == 0) {
        printf("L");
        return;
    }
    Node top = tree_get_node(tree, index);
    switch (node_get_type(top)) {
        case NODE_TYPE_INDIR: {
            _tree_print_comb_subtree(tree, node_get_indir(top), FALSE);
            break;
        }
        case NODE_TYPE_STEM: {
            printf("S");
            Index left = node_get_child(top, CHILD_SINGLE);
            _tree_print_comb_subtree(tree, left, FALSE);
            break;
        }
        case NODE_TYPE_FORK: {
            printf("F");
            Index left = node_get_child(top, CHILD_SIDE_LEFT);
            _tree_print_comb_subtree(tree, left, FALSE);

            Index right = node_get_child(top, CHILD_SIDE_RIGHT);
            _tree_print_comb_subtree(tree, right, FALSE);
            break;
        }
        case NODE_TYPE_APP: {
            printf("A");
            Index left = node_get_child(top, CHILD_SIDE_LEFT);
            _tree_print_comb_subtree(tree, left, FALSE);

            Index right = node_get_child(top, CHILD_SIDE_RIGHT);
            _tree_print_comb_subtree(tree, right, FALSE);
            break;
        }
        default: {
            fail("PANIC! tree_print: invalid tag - %d\n", node_get_type(top));
            break;
        }
    }
}

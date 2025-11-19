#include <stdio.h>

#include "tree.h"
#include "array.h"
#include "global.h"
#include "node.h"

void _tree_print_subtree(struct Tree tree, char* buffer, bool_t use_spaces,
    Index index, bool_t root);
void _tree_print_comb_subtree(struct Tree tree, Index index, bool_t root);
size_t _tree_get_node_count(struct Tree tree, Index index, bool_t root);

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

// Either allocate or reuse space for a new node, and fill it with data
Index tree_add_node(struct Tree* tree, enum NodeTag tag, Index left_child_index,
    Index right_child_index)
{
    // Increase refcounts for the children
    tree_incr_refcount(tree, left_child_index);
    tree_incr_refcount(tree, right_child_index);

    Node new_node = node_make(tag, 0, left_child_index, right_child_index);
    Index new_node_index = 0;

    // Check if the last node is a marker for empty space
    Index last_index = node_array_count(tree->nodes) - 1;
    if (last_index != 0) {
        Node last_node = tree_get_node(*tree, last_index);
        if (node_get_refcount(last_node) == 0) {
            new_node_index = node_get_indir(last_node);
            node_array_pop(&tree->nodes);
        }
    }

    if (new_node_index != 0) {
        tree_delete_children(tree, new_node_index);
        tree_set_node(tree, new_node_index, new_node);
        tree->free_space_count--;
    } else {
        new_node_index = node_array_push(&tree->nodes, new_node);
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

// Since this is used by the API, skip over indirection nodes
Node* tree_get_node_ref(struct Tree tree, Index index) {
    Node* node = node_array_get_ref(tree.nodes, index);
    if (node == NULL) {
        return NULL;
    }
    while (node_get_tag(*node) == Indirection) {
        Index referenced_node_index = node_get_indir(*node);
        if (referenced_node_index == 0) {
            return NULL;
        }
        node = node_array_get_ref(tree.nodes, referenced_node_index);
    }
    return node;
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
        tree->free_space_count++;
        node_array_push(&tree->nodes, node_make(0, 0, index, 0));
    }
}

void tree_delete_children(struct Tree* tree, Index index) {
    Node node = tree_get_node(*tree, index);
    switch (node_get_tag(node)) {
        case Stem: {
            Index left = node_get_left_child_index(node);
            tree_decr_refcount(tree, left);
            break;
        }
        case Fork:
        case App: {
            Index left = node_get_left_child_index(node);
            Index right = node_get_right_child_index(node);
            tree_decr_refcount(tree, left);
            tree_decr_refcount(tree, right);
            break;
        }
        default: {
            break;
        }
    }
}

size_t tree_get_node_count(struct Tree tree) {
    return _tree_get_node_count(tree, 0, TRUE);
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

// ----------------------------- INTERNAL METHODS -----------------------------

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

void _tree_print_subtree(struct Tree tree, char* buffer, bool_t use_spaces,
    Index index, bool_t root)
{
    if (root == FALSE && index == 0) {
        strcat(buffer, "t");
        return;
    }
    Node top = tree_get_node(tree, index);
    switch (node_get_tag(top)) {
        case Indirection: {
            _tree_print_subtree(tree, buffer, use_spaces, node_get_indir(top),
                FALSE);
            break;
        }
        case Stem: {
            strcat(buffer, "t");
            Index left = node_get_left_child_index(top);
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
        case Fork: {
            strcat(buffer, "t");
            Index left = node_get_left_child_index(top);
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
            Index right = node_get_right_child_index(top);
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
        case App: {
            Index left = node_get_left_child_index(top);
            _tree_print_subtree(tree, buffer, use_spaces, left, FALSE);
            Index right = node_get_right_child_index(top);
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
            fail("PANIC! tree_print: invalid tag - %d\n", node_get_tag(top));
            break;
        }
    }
}

void _tree_print_comb_subtree(struct Tree tree, Index index, bool_t root) {
    if (root == FALSE && index == 0) {
        printf("L");
        return;
    }
    Node top = tree_get_node(tree, index);
    switch (node_get_tag(top)) {
        case Indirection: {
            _tree_print_comb_subtree(tree, node_get_indir(top), FALSE);
            break;
        }
        case Stem: {
            printf("S");
            Index left = node_get_left_child_index(top);
            _tree_print_comb_subtree(tree, left, FALSE);
            break;
        }
        case Fork: {
            printf("F");
            Index left = node_get_left_child_index(top);
            _tree_print_comb_subtree(tree, left, FALSE);

            Index right = node_get_right_child_index(top);
            _tree_print_comb_subtree(tree, right, FALSE);
            break;
        }
        case App: {
            printf("A");
            Index left = node_get_left_child_index(top);
            _tree_print_comb_subtree(tree, left, FALSE);

            Index right = node_get_right_child_index(top);
            _tree_print_comb_subtree(tree, right, FALSE);
            break;
        }
        default: {
            fail("PANIC! tree_print: invalid tag - %d\n", node_get_tag(top));
            break;
        }
    }
}

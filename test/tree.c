#include "test.h"

#include "../src/tree.h"

void test() {
    char buf[1024];
    struct Tree tree = tree_make(1024);

    Index app_index = tree_add_node(&tree, NODE_TAG_APP);
    Index fork_index = tree_add_node(&tree, NODE_TAG_FORK);
    tree_change_child(&tree, app_index, CHILD_SIDE_LEFT, fork_index);
    check("Check tree length", node_array_count(tree.nodes) == 2);
    tree_print(tree, buf, FALSE);
    printf("Tree: %s\n", buf);
    tree_debug_print(tree);

    // printf("Tree print test:\n");
    // tree_print(tree);
    // tree_debug_print(tree);

    tree_copy_child(&tree, app_index, CHILD_SIDE_LEFT, app_index,
        CHILD_SIDE_RIGHT);
    Node app_node = tree_get_node(tree, app_index);
    check("Check node copy - tag of App's left child",
        node_get_tag(tree_get_node(tree, node_get_child_index(app_node,
            CHILD_SIDE_LEFT))) == NODE_TAG_FORK);
    check("Check node copy - index of App's right child",
        node_get_child_index(app_node, CHILD_SIDE_LEFT) != 0);
    check("Check node copy - refcount of App's left child",
        node_get_refcount(tree_get_node(tree, node_get_child_index(app_node,
            CHILD_SIDE_LEFT))) == 2);
    tree_detach_child(&tree, app_index, CHILD_SIDE_LEFT);
    app_node = tree_get_node(tree, app_index);
    check("Check node detach - index of App's left child",
        node_get_child_index(app_node, CHILD_SIDE_LEFT) == 0);
    check("Check node detach - index of App's right child",
        node_get_child_index(app_node, CHILD_SIDE_RIGHT) != 0);
    check("Check node detach - refcount of App's right child",
        node_get_refcount(tree_get_node(tree, node_get_child_index(app_node,
            CHILD_SIDE_RIGHT))) == 1);
}

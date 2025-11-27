#include "test.h"

#include "../src/tree.h"

void test() {
    char buf[1024];
    struct Tree tree = tree_make(1024);

    Node node_fork = node_make(NODE_TYPE_FORK, 1, 0, 0);
    Index fork_index = tree_add_node(&tree, node_fork);
    Node node_app = node_make(NODE_TYPE_APP, 1, fork_index, 0);
    Index app_index = tree_add_node(&tree, node_app);
    check("Check tree length", node_array_count(tree.nodes) == 2);
    tree_print(tree, buf, FALSE);
    printf("Tree: %s\n", buf);
    tree_debug_print(tree);

    // printf("Tree print test:\n");
    // tree_print(tree);
    // tree_debug_print(tree);

    node_app = node_set_child(node_app, CHILD_SIDE_RIGHT, app_index);
    tree_set_node(tree, app_index, node_app);
    check("Check node copy - tag of App's left child",
        node_get_type(tree_get_node(tree, node_get_child(node_app,
            CHILD_SIDE_LEFT))) == NODE_TYPE_FORK);
}

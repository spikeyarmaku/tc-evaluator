#include "test.h"

#include "../tree.h"

void tree_test() {
    struct Tree tree = tree_make();

    tree_add_node(&tree, App, 1, 0);
    tree_add_node(&tree, Fork, 0, 0);
    check("Check tree length", node_array_count(tree.nodes) == 2);

    // printf("Tree print test:\n");
    // tree_print(tree);
    // tree_debug_print(tree);

    tree_decr_refcount(&tree, 1);
    tree_add_node(&tree, Fork, 2, 0);
    tree_add_node(&tree, Stem, 0, 0);
    check("Check tree length",
        node_array_count(tree.nodes) - tree.free_space_count == 3);
    
    check("Check tree node count", tree_get_node_count(tree) == 5);
}

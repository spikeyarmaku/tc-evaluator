// #include "runtime.h"
#include "src/bintree/runtime.h"

int main() {
    printf("Initializing stack and tree\n");
    struct Tree* tree = tree_make();
    printf("Initializing program\n");
    tree->root = init_program(tree);
    draw_tree(".output/output_tree", tree);

    
    printf("Starting reduction\n");
    struct Reduct reduct = reduce(0, tree, &tree->root, 0);
    print_reduct(0, reduct);

    print_tree(0, tree);
    pretty_print(tree->root);
    return 0;
}


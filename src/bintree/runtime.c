// #include "runtime.h"
#include "src/bintree/runtime.h"

int main() {
    printf("Initializing stack and tree\n");
    struct Tree* tree = tree_make();
    printf("Initializing program\n");
    tree->root = init_program(tree);
    pretty_print(tree->root);
    print_tree(0, tree);
    draw_tree(".output/output_tree", tree);
    
    printf("Starting reduction\n");
    struct VM* vm = vm_make(tree);
    vm_run(vm);

    print_tree(0, tree);
    pretty_print(tree->root);
    return 0;
}


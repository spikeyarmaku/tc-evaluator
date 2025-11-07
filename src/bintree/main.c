#include "main.h"
#include "init.h"
#include "tree.h"

int main() {
    printf("Initializing tree\n");
    struct Tree tree = tree_make();
    printf("Initializing program\n");
    init_program(&tree);
    tree_print_comb(tree);
    // tree_debug_print(tree);

    printf("Starting reduction\n");
    struct VM vm = vm_make(tree);
    vm_run(&vm);

    // tree_print_comb(vm.tree);
    tree_print(vm.tree);
    // tree_debug_print(vm.tree);
    return 0;
}

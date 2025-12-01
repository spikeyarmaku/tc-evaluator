#include <stdio.h>
#include <stdlib.h>

#include "../include/tceval.h"

#define INDENT_SIZE 4

struct UserData {
    char magic[4];
};

void print_indented(int amount, char* str) {
    for (int i = 0; i < amount; i++) {
        printf(" ");
    }
    printf("%s", str);
}

void print_tree(Vm_h vm, Node_h node, int indent_level) {
    switch (tc_get_node_type(node)) {
        case NODE_TYPE_LEAF: {
            printf("Leaf\n");
            break;
        }
        case NODE_TYPE_STEM: {
            printf("Stem\n");
            print_indented((indent_level + 1) * INDENT_SIZE, "└ ");
            print_tree(vm, tc_get_node(vm, tc_get_node_child(node,
                CHILD_SIDE_LEFT)), indent_level + 1);
            break;
        }
        case NODE_TYPE_FORK:
        case NODE_TYPE_APP: {
            if (tc_get_node_type(node) == NODE_TYPE_FORK) {
                printf("Fork\n");
            } else {
                printf("App\n");
            }
            print_indented((indent_level + 1) * INDENT_SIZE, "├ ");
            print_tree(vm, tc_get_node(vm, tc_get_node_child(node,
                CHILD_SIDE_LEFT)), indent_level + 1);
            print_indented((indent_level + 1) * INDENT_SIZE, "└ ");
            print_tree(vm, tc_get_node(vm, tc_get_node_child(node,
                CHILD_SIDE_RIGHT)), indent_level + 1);
            break;
        }
        default: {
            break;
        }
    }
}

size_t read_vm(void* ctx, void* data, size_t size) {
    FILE* fp = ctx;
    return fread(data, 1, size, fp);
}

size_t write_vm(void* ctx, void* data, size_t size) {
    FILE* fp = ctx;
    return fwrite(data, 1, size, fp);
}

int main() {
    // This is an example code to demonstrate the usage of this lib
    printf("=== Example TC program ===\n");

    // First, we create a VM
    printf("> Create VM\n");
    Vm_h vm;
    enum VmResult result = tc_make_vm(&vm, vm_default_config);
    if (result != VM_OK) {
        printf("Error: %d\n", result);
        exit(EXIT_FAILURE);
    }

    // Then we add a few nodes manually - the tree we are going to define should
    // invoke reduction rule 2
    printf("> Add nodes\n");
    Index index;
    index = tc_add_leaf(vm);
    index = tc_add_stem(vm, index);
    index = tc_add_fork(vm, index, 0);
    // A node can be referenced more than once:
    index = tc_add_app(vm, index, index);
    // After nodes are added, we must tell which one is the root of the tree
    tc_set_top(vm, index);
    // Print the tree to check if everything is correct
    print_tree(vm, tc_get_top(vm), 0);
    tc_debug_print_tree(vm);

    // Now we save the VM to a file. In order to do this, the library asks for a
    // callback. Our callback will get a file via the context pointer, and
    // writes to the file with a given chunk size (1<<10 = 1024 bytes).
    printf("> Save VM to file\n");
    FILE *fp = fopen("vm.img", "wb");
    struct UserData user_data = {"USER"};
    size_t chunk_size = 1<<10;
    tc_write_vm(vm, &user_data, sizeof(struct UserData), chunk_size, write_vm,
        fp);
    fclose(fp);
    // Then we null out the VM handle
    tc_free_vm(vm);
    vm = NULL;

    // Next, we read the VM from the file we have just written to. Similarly to
    // the previous case, the library asks for a callback, but this time, we
    // read the chunks of the given size (1<<10 = 1024 bytes) from the file.
    printf("> Load VM from file\n");
    fp = fopen("vm.img", "rb");

    struct VmHeader header;
    result = tc_read_vm_header(&header, chunk_size, read_vm, fp);
    if (result != VM_OK) {
        printf("Error: %d\n", result);
        exit(EXIT_FAILURE);
    }
    result = tc_read_user_data(&header, &user_data, read_vm, fp);
    if (result != VM_OK) {
        printf("Error: %d\n", result);
        exit(EXIT_FAILURE);
    }
    // Here user data (e.g. language backend version) can be checked, and vm
    // data readout can be aborted if necessary
    result = tc_read_vm_data(&header, &vm, chunk_size, read_vm, fp);
    if (result != VM_OK) {
        printf("Error: %d\n", result);
        exit(EXIT_FAILURE);
    }

    // After it is done, reduce until there's nothing more to reduce
    printf("> Run VM\n");
    tc_run(vm);

    // Lastly, print the resulting tree
    printf("> Query the result\n");
    // We will start from the top, so we need to query it
    Node_h top = tc_get_top(vm);
    print_tree(vm, top, 0);
    tc_debug_print_tree(vm);
}

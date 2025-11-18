#include <stdio.h>

#include "../include/tceval.h"

int main() {
    printf("Example TC program\n");
    VM_h vm;
    tc_make_vm(&vm, tc_default_config);
    Index index = tc_add_node(vm, NODE_TYPE_APP, 0, 0);
    index = tc_add_node(vm, NODE_TYPE_APP, index, 0);
    tc_set_top(vm, index);
    tc_run(vm);
    Node_h node = tc_get_top(vm);
    printf("Top node: %d\n", tc_get_node_type(node));
    printf("Left child: %d\n", tc_get_node_type(tc_get_node(vm, tc_get_left(
        node))));
    printf("Right child: %d\n", tc_get_node_type(tc_get_node(vm, tc_get_right(
        node))));
}

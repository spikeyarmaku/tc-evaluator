#include "../include/tceval.h"
#include "node.h"
#include "tree.h"
#include "vm.h"

const struct VMConfig tc_default_config = {1<<10, 1<<10};

VM_h tc_make_vm(struct VMConfig config) {
    struct VM* vm = malloc(sizeof(struct VM));
    *vm = vm_make(config);
    return vm;
}

void tc_free_vm(VM_h vm) {
    vm_free(*vm);
}

void tc_load_tree(VM_h vm, uint8_t* bytes, size_t size) {
    // TODO
}

uint8_t* tc_save_tree(VM_h vm, size_t* size) {
    // TODO
}

Index tc_add_node(VM_h vm, enum NodeType type, Index left, Index right) {
    if (type == NODE_TYPE_LEAF) {
        return 0;
    }
    return tree_add_node(&vm->tree, (enum NodeTag)type, left, right);
}

enum StepState tc_step(VM_h vm) {
    return vm_step(vm);
}

void tc_run(VM_h vm) {
    vm_run(vm);
}

Node_h tc_get_top(VM_h vm) {
    return tree_get_node_ref(vm->tree, 0);
}

void tc_set_top(VM_h vm, Index index) {
    Node node = node_make_empty();
    node_set_indir(&node, index);
    tree_set_node(&vm->tree, 0, node);
}

Node_h tc_get_node(VM_h vm, Index index) {
    if (index == 0) {
        return NULL;
    }
    return tree_get_node_ref(vm->tree, index);
}

enum NodeType tc_get_node_type(Node_h node) {
    if (node == NULL) {
        return NODE_TYPE_LEAF;
    }
    return (enum NodeType)node_get_tag(*node);
}

Index tc_get_left(Node_h node) {
    if (node == NULL) {
        fail("tc_get_left: node == NULL\n");
    }
    Index left = node_get_left_child_index(*node);
}

Index tc_get_right(Node_h node) {
    if (node == NULL) {
        fail("tc_get_right: node == NULL\n");
    }
    return node_get_right_child_index(*node);
}

void tc_debug_print_tree(VM_h vm) {
    tree_debug_print(vm->tree);
}

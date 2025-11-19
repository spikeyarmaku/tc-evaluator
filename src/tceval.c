#include "../include/tceval.h"
#include "node.h"
#include "tree.h"
#include "vm.h"

extern const struct VMConfig vm_default_config;

enum VMResult tc_make_vm(VM_h* vm, struct VMConfig config) {
    *vm = malloc(sizeof(struct VM));
    **vm = vm_make(config);
    return VM_OK;
}

void tc_free_vm(VM_h vm) {
    vm_free(vm);
    free(vm);
}

enum VMResult tc_read_vm(VM_h* vm, vm_read_fn fn, size_t chunk_size, void* ctx)
{
    *vm = malloc(sizeof(struct VM));
    return vm_deserialize(*vm, fn, chunk_size, ctx);
}

void tc_write_vm(VM_h vm, vm_write_fn fn, size_t chunk_size, void* ctx) {
    vm_serialize(fn, *vm, chunk_size, ctx);
}

size_t tc_vm_get_size(VM_h vm) {
    return vm_get_size(*vm);
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
    vm_init(vm);
    vm_run(vm);
}

Node_h tc_get_top(VM_h vm) {
    return tree_get_node_ref(vm->tree, 0);
}

void tc_set_top(VM_h vm, Index index) {
    Node node = tree_get_node(vm->tree, 0);
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
    return node_get_left_child_index(*node);
}

Index tc_get_right(Node_h node) {
    return node_get_right_child_index(*node);
}

void tc_debug_print_tree(VM_h vm) {
    tree_debug_print(vm->tree);
}

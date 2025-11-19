#include "../include/tceval.h"
#include "node.h"
#include "tree.h"
#include "vm.h"

extern const struct VmConfig vm_default_config;

enum VmResult tc_make_vm(Vm_h* vm, struct VmConfig config) {
    *vm = malloc(sizeof(struct Vm));
    **vm = vm_make(config);
    return VM_OK;
}

void tc_free_vm(Vm_h vm) {
    vm_free(vm);
    free(vm);
}

enum VmResult tc_read_vm(Vm_h* vm, vm_read_fn fn, size_t chunk_size, void* ctx)
{
    *vm = malloc(sizeof(struct Vm));
    return vm_deserialize(*vm, fn, chunk_size, ctx);
}

void tc_write_vm(Vm_h vm, vm_write_fn fn, size_t chunk_size, void* ctx) {
    vm_serialize(fn, *vm, chunk_size, ctx);
}

size_t tc_vm_get_size(Vm_h vm) {
    return vm_get_size(*vm);
}

Index tc_add_node(Vm_h vm, enum NodeType type, Index left, Index right) {
    if (type == NODE_TYPE_LEAF) {
        return 0;
    }
    return tree_add_node(&vm->tree, (enum NodeTag)type, left, right);
}

enum StepState tc_step(Vm_h vm) {
    return vm_step(vm);
}

void tc_run(Vm_h vm) {
    vm_init(vm);
    vm_run(vm);
}

Node_h tc_get_top(Vm_h vm) {
    return tree_get_node_ref(vm->tree, 0);
}

void tc_set_top(Vm_h vm, Index index) {
    Node node = tree_get_node(vm->tree, 0);
    node_set_indir(&node, index);
    tree_set_node(&vm->tree, 0, node);
}

Node_h tc_get_node(Vm_h vm, Index index) {
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

// void tc_compact_vm(Vm_h vm) {
//     vm_compact(vm);
// }

void tc_debug_print_tree(Vm_h vm) {
    tree_debug_print(vm->tree);
}

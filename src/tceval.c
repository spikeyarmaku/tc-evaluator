#include "../include/tceval.h"
#include "node.h"
#include "tree.h"
#include "vm.h"

extern const struct VmConfig vm_default_config;

static Index _tc_add_node(Vm_h vm, enum NodeType type, Index left, Index right);

enum VmResult tc_make_vm(Vm_h* vm, struct VmConfig config) {
    *vm = malloc(sizeof(struct Vm));
    if (*vm == NULL) {
        return VM_ERR_OOM;
    }
    **vm = vm_make(config);
    return VM_OK;
}

void tc_free_vm(Vm_h vm) {
    vm_free(vm);
    free(vm);
}

enum VmResult tc_read_vm_header(struct VmHeader* header_h,
    size_t chunk_size, vm_read_fn read_fn, void* ctx)
{
    return vm_read_header(header_h, chunk_size, read_fn, ctx);
}

enum VmResult tc_read_user_data(const struct VmHeader* header_h,
    void* user_data_h, vm_read_fn read_fn, void* ctx)
{
    return vm_read_user_data(header_h, user_data_h, read_fn, ctx);
}

enum VmResult tc_read_vm_data(const struct VmHeader* header_h,
    Vm_h* vm_h, size_t chunk_size, vm_read_fn read_fn, void* ctx)
{
    *vm_h = malloc(sizeof(struct Vm));
    return vm_read_vm_data(header_h, *vm_h, chunk_size, read_fn, ctx);
}

void tc_write_vm(Vm_h vm, void* user_data, uint16_t user_data_size,
    size_t chunk_size, vm_write_fn write_fn, void* ctx)
{
    vm_write(*vm, user_data, user_data_size, chunk_size, write_fn, ctx);
}

size_t tc_get_vm_size(Vm_h vm) {
    return vm_get_size(*vm);
}

static Index _tc_add_node(Vm_h vm, enum NodeType type, Index left, Index right) {
    if (type == NODE_TYPE_LEAF) {
        return 0;
    }
    Node node = node_make(type, 0, left, right);
    Index index = tree_add_node(&vm->tree, node);
    tree_incr_refcount(&vm->tree, left);
    tree_incr_refcount(&vm->tree, right);
    return index;
}

Index tc_add_app(Vm_h vm, Index left, Index right) {
    return _tc_add_node(vm, NODE_TYPE_APP, left, right);
}

Index tc_add_fork(Vm_h vm, Index left, Index right) {
    return _tc_add_node(vm, NODE_TYPE_FORK, left, right);
}

Index tc_add_stem(Vm_h vm, Index child) {
    return _tc_add_node(vm, NODE_TYPE_STEM, 0, child);
}

Index tc_leaf() {
    return 0;
}

enum VmState tc_step(Vm_h vm) {
    return vm_step(vm);
}

void tc_run(Vm_h vm) {
    vm_run(vm);
}

int tc_can_run(Vm_h vm) {
    Node current = tree_get_node(vm->tree, 0);
    while (node_get_type(current) == NODE_TYPE_INDIR) {
        current = tree_get_node(vm->tree, node_get_indir(current));
    }
    // TODO handle custom nodes
    if (node_get_type(current) == NODE_TYPE_APP) {
        return 1;
    } else {
        return 0;
    }
}

Index tc_get_top(Vm_h vm) {
    return node_get_indir(tree_get_node(vm->tree, 0));
}

void tc_set_top(Vm_h vm, Index index) {
    tree_decr_refcount(&vm->tree, node_get_indir(tree_get_node(vm->tree, 0)));
    tree_set_node(vm->tree, 0, node_make(NODE_TYPE_INDIR, 1, 0, index));
    tree_incr_refcount(&vm->tree, index);
}

enum NodeType tc_get_node_type(Vm_h vm, Index index) {
    if (index == 0) {
        return NODE_TYPE_LEAF;
    }
    return node_get_type(tree_get_node(vm->tree, index));
}

Index tc_get_node_child(Vm_h vm, Index index, enum ChildSide side) {
    return node_get_child(tree_get_node(vm->tree, index), side);
}

void tc_set_node_child(Vm_h vm, Index index, enum ChildSide side,
    Index new_child_index)
{
    Node node = tree_get_node(vm->tree, index);
    Index old_child_index = node_get_child(node, side);
    tree_set_node(vm->tree, index, node_set_child(node, side, new_child_index));
    tree_decr_refcount(&vm->tree, old_child_index);
    tree_incr_refcount(&vm->tree, new_child_index);
}

// void tc_compact_vm(Vm_h vm) {
//     vm_compact(vm);
// }

void tc_debug_print_tree(Vm_h vm) {
    tree_debug_print(vm->tree);
}

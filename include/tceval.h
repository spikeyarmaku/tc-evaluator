#ifndef TCEVAL_H
#define TCEVAL_H

#include <stdlib.h>

// --- Types ---

typedef size_t (*vm_read_fn)(void* ctx, void* data, size_t size);
typedef size_t (*vm_write_fn)(void* ctx, void* data, size_t size);

struct VmConfig {
    size_t initial_tree_capacity;
    size_t initial_spine_capacity;
};

typedef struct Vm* Vm_h;
typedef struct NodeData* Node_h;
typedef size_t Index;

enum NodeType {
    NODE_TYPE_LEAF,
    NODE_TYPE_STEM,
    NODE_TYPE_FORK,
    NODE_TYPE_APP
};

enum VmResult {
    VM_OK = 0,
    VM_ERR_MAGIC_MISMATCH,
    VM_ERR_UNSUPPORTED_VERSION,
    VM_ERR_TRUNCATED
};

// --- Constants ---

extern const struct VmConfig vm_default_config;

// --- Methods ---

// Create VM
enum VmResult   tc_make_vm      (Vm_h* vm, struct VmConfig config);
enum VmResult   tc_read_vm      (Vm_h* vm, vm_read_fn fn, size_t chunk_size,
    void* ctx);

// Delete VM
void            tc_free_vm      (Vm_h vm);

// Serialize VM
void            tc_write_vm     (Vm_h vm, vm_write_fn fn, size_t chunk_size,
    void* ctx);
size_t          tc_vm_get_size  (Vm_h vm);

// Add a node manually
Index           tc_add_node     (Vm_h vm, enum NodeType type, Index left,
    Index right);

// Reduction methods
enum StepState  tc_step         (Vm_h vm); // Evaluate one step
void            tc_run          (Vm_h vm); // Run evaluation to the end

// Query methods
Node_h          tc_get_top      (Vm_h vm); // Get the top node of the tree
void            tc_set_top      (Vm_h vm, Index index);
Node_h          tc_get_node     (Vm_h vm, Index index); // Get node by index
enum NodeType   tc_get_node_type    (Node_h node); // Get a node's type tag
// Get the index of a node's child
Index           tc_get_left     (Node_h node);
Index           tc_get_right    (Node_h node);

// Misc
// void            tc_compact_vm   (Vm_h vm);
void            tc_debug_print_tree (Vm_h vm);

#endif

#ifndef TCEVAL_H
#define TCEVAL_H

#include <stdint.h>
#include <stdlib.h>

// --- Types ---

typedef size_t (*vm_read_fn)(void* ctx, void* data, size_t size);
typedef size_t (*vm_write_fn)(void* ctx, void* data, size_t size);

struct VMConfig {
    size_t initial_tree_capacity;
    size_t initial_spine_capacity;
};

typedef struct VM* VM_h;
typedef struct NodeData* Node_h;
typedef size_t Index;

enum NodeType {
    NODE_TYPE_LEAF,
    NODE_TYPE_STEM,
    NODE_TYPE_FORK,
    NODE_TYPE_APP
};

enum VMResult {
    VM_OK = 0,
    VM_ERR_MAGIC_MISMATCH,
    VM_ERR_UNSUPPORTED_VERSION,
    VM_ERR_TRUNCATED
};

// --- Constants ---

extern const struct VMConfig vm_default_config;

// --- Methods ---

// Create VM
enum VMResult   tc_make_vm      (VM_h* vm, struct VMConfig config);
enum VMResult   tc_read_vm      (VM_h* vm, vm_read_fn fn, size_t chunk_size,
    void* ctx);

// Delete VM
void            tc_free_vm      (VM_h vm);

// Serialize VM
void            tc_write_vm     (VM_h vm, vm_write_fn fn, size_t chunk_size,
    void* ctx);
size_t          tc_vm_get_size  (VM_h vm);

// Add a node manually
Index           tc_add_node     (VM_h vm, enum NodeType type, Index left,
    Index right);

// Reduction methods
enum StepState  tc_step         (VM_h vm); // Evaluate one step
void            tc_run          (VM_h vm); // Run evaluation to the end

// Query methods
Node_h          tc_get_top      (VM_h vm); // Get the top node of the tree
void            tc_set_top      (VM_h vm, Index index);
Node_h          tc_get_node     (VM_h vm, Index index); // Get node by index
enum NodeType   tc_get_node_type    (Node_h node); // Get a node's type tag
// Get the index of a node's child
Index           tc_get_left     (Node_h node);
Index           tc_get_right    (Node_h node);

// Misc
void            tc_debug_print_tree (VM_h vm);

#endif

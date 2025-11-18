#ifndef TCEVAL_H
#define TCEVAL_H

#include <stdint.h>
#include <stdlib.h>

// --- Types ---

typedef size_t (*vm_read_fn)(void* data, size_t size);
typedef size_t (*vm_write_fn)(void* data, size_t size);

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

extern const struct VMConfig tc_default_config;

// --- Methods ---

// Create a new VM
enum VMResult   tc_make_vm      (VM_h* vm, struct VMConfig config);

// Delete VM
void            tc_free_vm      (VM_h vm);

enum VMResult   tc_read_vm      (VM_h* vm, vm_read_fn fn, uint8_t* bytes,
    size_t chunk_size);

void            tc_write_vm     (vm_write_fn fn, VM_h vm, size_t chunk_size);

size_t          tc_vm_get_size  (VM_h vm);

// Add a node manually
Index           tc_add_node     (VM_h vm, enum NodeType type, Index left,
    Index right);

// Evaluate one step
enum StepState  tc_step         (VM_h vm);

// Run evaluation to the end
void            tc_run          (VM_h vm);

// Get the top node of the tree
Node_h          tc_get_top      (VM_h vm);

void            tc_set_top      (VM_h vm, Index index);

// Get a node by its index
Node_h          tc_get_node     (VM_h vm, Index index);

// Get a node's type tag
enum NodeType   tc_get_node_type    (Node_h node);

// Get the index of a node's child
Index           tc_get_left     (Node_h node);
Index           tc_get_right    (Node_h node);

void            tc_debug_print_tree (VM_h vm);

#endif

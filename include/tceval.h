#ifndef TCEVAL_H
#define TCEVAL_H

#include <stdint.h>
#include <stdlib.h>

struct VMConfig {
    size_t initial_tree_capacity;
    size_t initial_spine_capacity;
};

extern const struct VMConfig tc_default_config;

typedef struct VM* VM_h;
typedef struct NodeData* Node_h;
typedef size_t Index;

enum NodeType {
    NODE_TYPE_LEAF,
    NODE_TYPE_STEM,
    NODE_TYPE_FORK,
    NODE_TYPE_APP
};

// Create a new VM
VM_h            tc_make_vm          (struct VMConfig config);

// Delete VM
void            tc_free_vm          (VM_h vm);

// Load a tree from a byte array
void            tc_load_tree        (VM_h vm, uint8_t* bytes, size_t size);

// Save the current tree to a byte array, and reporting the number of bytes
// written
uint8_t*        tc_save_tree        (VM_h vm, size_t* size);

// Add a node manually
Index           tc_add_node         (VM_h vm, enum NodeType type, Index left,
    Index right);

// Evaluate one step
enum StepState  tc_step             (VM_h vm);

// Run evaluation to the end
void            tc_run              (VM_h vm);

// Get the top node of the tree
Node_h          tc_get_top          (VM_h vm);

void            tc_set_top          (VM_h vm, Index index);

// Get a node by its index
Node_h          tc_get_node         (VM_h vm, Index index);

// Get a node's type tag
enum NodeType   tc_get_node_type    (Node_h node);

// Get the index of a node's child
Index           tc_get_left         (Node_h node);
Index           tc_get_right        (Node_h node);

void            tc_debug_print_tree (VM_h vm);

#endif

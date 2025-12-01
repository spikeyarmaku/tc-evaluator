#ifndef TCEVAL_H
#define TCEVAL_H

#include <stdlib.h>
#include <stdint.h>

// --- Types ---

typedef size_t (*vm_read_fn)(void* ctx, void* data, size_t size);
typedef size_t (*vm_write_fn)(void* ctx, void* data, size_t size);

struct VmConfig {
    size_t initial_tree_capacity;
    size_t initial_spine_capacity;
};

typedef struct Vm* Vm_h;
typedef size_t Index;

enum NodeType {
    NODE_TYPE_CUSTOM = 0, // User defined
    NODE_TYPE_STEM   = 1,
    NODE_TYPE_FORK   = 2,
    NODE_TYPE_APP    = 3,
    NODE_TYPE_INDIR  = 4,
    NODE_TYPE_LEAF   = 5
};

struct VmHeader {
    char magic[4];              // 4 bytes
    uint8_t version_major;      // 1 byte
    uint8_t version_minor;      // 1 byte
    uint16_t user_data_size;    // 2 bytes - size of the user-defined data
    size_t size;                // 8 bytes - size of the node array in bytes
};                              // TOTAL: 16 bytes

enum VmResult {
    VM_OK = 0,
    VM_ERR_MAGIC_MISMATCH,
    VM_ERR_UNSUPPORTED_VERSION,
    VM_ERR_TRUNCATED,
    VM_ERR_OOM,
    VM_ERR_INTERNAL
};

enum ChildSide {
    CHILD_SIDE_LEFT,
    CHILD_SIDE_RIGHT,
    CHILD_SINGLE
};

// --- Constants ---

extern const struct VmConfig vm_default_config;

// --- Methods ---

// Create VM
enum VmResult   tc_make_vm          (Vm_h* vm, struct VmConfig config);
enum VmResult   tc_read_vm_header   (struct VmHeader* header_h,
    size_t chunk_size, vm_read_fn read_fn, void* ctx);
enum VmResult   tc_read_user_data   (const struct VmHeader* header_h,
    void* user_data_h, vm_read_fn read_fn, void* ctx);
enum VmResult   tc_read_vm_data     (const struct VmHeader* header_h,
    Vm_h* vm_h, size_t chunk_size, vm_read_fn read_fn, void* ctx);

// Delete VM
void            tc_free_vm      (Vm_h vm);

// Serialize VM
void            tc_write_vm     (Vm_h vm, void* user_data,
    uint16_t user_data_size, size_t chunk_size, vm_write_fn write_fn,
    void* ctx);
size_t          tc_get_vm_size  (Vm_h vm);

// Add a node manually
Index           tc_add_app      (Vm_h vm, Index left, Index right);
Index           tc_add_fork     (Vm_h vm, Index left, Index right);
Index           tc_add_stem     (Vm_h vm, Index child);
Index           tc_leaf         ();
// Index           tc_add_custom   (Vm_h vm, size_t left, size_t right);

// Reduction methods
enum VmState    tc_step         (Vm_h vm); // Evaluate one step
void            tc_run          (Vm_h vm); // Run evaluation to the end

// Query methods
Index           tc_get_top      (Vm_h vm);
void            tc_set_top      (Vm_h vm, Index index);
enum NodeType   tc_get_node_type    (Vm_h vm, Index index);
// Get the index of a node's child
Index           tc_get_node_child   (Vm_h vm, Index index, enum ChildSide side);
void            tc_set_node_child   (Vm_h vm, Index index, enum ChildSide side,
    Index new_child_index);

// Misc
// void            tc_compact_vm   (Vm_h vm);
void            tc_debug_print_tree (Vm_h vm);

#endif

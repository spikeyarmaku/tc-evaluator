#ifndef VM_H
#define VM_H

#include "tree.h"
#include "../include/shrubble.h"

enum VmState {
    VM_STATE_DONE,
    VM_STATE_RUNNING
};

struct Vm {
    struct Tree tree; // Node array
    struct Array spine; // Index array
};

extern const struct VmConfig vm_default_config;

struct Vm       vm_make         (struct VmConfig config);
void            vm_free         (struct Vm* vm);
enum VmState    vm_step         (struct Vm* vm);
void            vm_run          (struct Vm* vm);
// void            vm_compact      (struct Vm* vm);
size_t          vm_get_size     (struct Vm vm);
void            vm_spine_print  (struct Array spine);

// Serialize / deserialize
void            vm_write            (struct Vm vm, void* user_data,
    uint16_t user_data_size, size_t chunk_size, vm_write_fn write_fn,
    void* ctx);
enum VmResult   vm_read_header      (struct VmHeader* header_h,
    size_t chunk_size, vm_read_fn read_fn, void* ctx);
enum VmResult   vm_read_user_data   (const struct VmHeader* header_h,
    void* user_data_h, vm_read_fn read_fn, void* ctx);
enum VmResult   vm_read_vm_data     (const struct VmHeader* header_h,
    struct Vm* vm_h, size_t chunk_size, vm_read_fn read_fn, void* ctx);

#endif

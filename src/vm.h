#ifndef VM_H
#define VM_H

#include "tree.h"
#include "../include/tceval.h"

enum StepState {Done, Running};

struct Vm {
    struct Tree tree; // Node array
    struct Array spine; // Index array
};

struct VmHeader {
    char magic[4];      // 4 bytes
    uint32_t version;   // 4 bytes
    size_t size;        // 8 bytes - size of the tree node array in bytes
};

struct Vm       vm_make         (struct VmConfig config);
void            vm_free         (struct Vm* vm);
void            vm_init         (struct Vm* vm);
enum StepState  vm_step         (struct Vm* vm);
void            vm_run          (struct Vm* vm);
size_t          vm_get_size     (struct Vm vm);
void            vm_serialize    (vm_write_fn write_fn, struct Vm vm,
    size_t chunk_size, void* ctx);
enum VmResult   vm_deserialize  (struct Vm* vm, vm_read_fn read_fn,
    size_t chunk_size, void* ctx);

#endif

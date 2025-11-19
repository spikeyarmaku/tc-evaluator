#ifndef VM_H
#define VM_H

#include "tree.h"
#include "../include/tceval.h"

enum StepState {Done, Running};

struct VM {
    struct Tree tree; // Node array
    struct Array spine; // Index array
};

struct VMHeader {
    char magic[4];      // 4 bytes
    uint32_t version;   // 4 bytes
    size_t size;        // 8 bytes - size of the tree node array in bytes
};

struct VM       vm_make         (struct VMConfig config);
void            vm_free         (struct VM* vm);
void            vm_init         (struct VM* vm);
enum StepState  vm_step         (struct VM* vm);
void            vm_run          (struct VM* vm);
size_t          vm_get_size     (struct VM vm);
void            vm_serialize    (vm_write_fn write_fn, struct VM vm,
    size_t chunk_size, void* ctx);
enum VMResult   vm_deserialize  (struct VM* vm, vm_read_fn read_fn,
    size_t chunk_size, void* ctx);

#endif

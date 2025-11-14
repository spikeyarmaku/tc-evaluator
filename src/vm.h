#ifndef VM_H
#define VM_H

#include "tree.h"
#include "../include/tceval.h"

enum StepState {Done, Running};

struct VM {
    struct Tree tree; // Node array
    struct Array spine; // Index array
};

struct VM       vm_make         (struct VMConfig config);
void            vm_free         (struct VM vm);
void            vm_init         (struct VM vm);
enum StepState  vm_step         (struct VM* vm);
void            vm_run          (struct VM* vm);
void*           vm_serialize    (struct VM vm, size_t* size);
struct VM       vm_deserialize  (void* data);

#endif

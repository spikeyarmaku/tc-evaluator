#ifndef REDUCTION_H
#define REDUCTION_H

#include "debug.h"
#include "tree.h"

enum StepState {Done, Running};

struct VM*      vm_make (struct Tree* tree);
enum StepState  vm_step (struct VM* vm);
void            vm_run  (struct VM* vm);
void*           vm_serialize    (struct VM* vm, size_t* size);
struct VM*      vm_deserialize  (void* data);

#endif
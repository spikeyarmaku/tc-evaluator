#ifndef REDUCTION_H
#define REDUCTION_H

#include "debug.h"
#include "tree.h"

struct VM*  vm_make (struct Tree* tree);
bool_t      vm_step (struct VM* vm);
void        vm_run  (struct VM* vm);

#endif
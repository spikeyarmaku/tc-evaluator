#ifndef MAIN_H
#define MAIN_H

#include <assert.h>

#include "tree.h"
#include "vm.h"

// This is going to be defined by the transpiler
void init_program(struct Tree* tree);

int main();

#endif

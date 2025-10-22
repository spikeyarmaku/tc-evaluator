#ifndef MAIN_H
#define MAIN_H

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <assert.h>

#include "tree.h"
#include "vm.h"
#include "debug.h"

// This is going to be defined by the transpiler
void init_program(struct Tree* tree);

int main();

#endif
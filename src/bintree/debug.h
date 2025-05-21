#ifndef DEBUG_H
#define DEBUG_H

// #define DEBUG_PRINTS

#ifdef DEBUG_PRINTS
#include <stdarg.h>
#include <stdio.h>
#include <inttypes.h>
#endif

#include <limits.h>

void debug(const char* s, ...);
void debug_indent(int indent_amount, const char* s, ...);
void printbits(unsigned long n);

#endif
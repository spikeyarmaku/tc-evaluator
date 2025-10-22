#ifndef DEBUG_H
#define DEBUG_H

#define DEBUG_PRINTS
// #define OUTPUT_DIAGRAMS

#ifdef DEBUG_PRINTS
#include <stdarg.h>     // vsprintf
#include <stdio.h>
#include <stdint.h>     // uint_least64_t
#include <inttypes.h>   // PRIuPTR
#endif

#include <limits.h>

#include "global.h"

void debug(const char* s, ...);
void debug_indent(int indent_amount, const char* s, ...);
void sprintbits(char* buffer, uint_least64_t n, bool_t sep);

#endif
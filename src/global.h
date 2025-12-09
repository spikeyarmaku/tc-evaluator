#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdint.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

typedef int bool_t;
typedef size_t Index;

#define TRUE 1
#define FALSE 0

#define REFCOUNT_LIMIT (((uint_least64_t)1<<30) - 1)
#define CHILD_LIMIT (((uint_least64_t)1<<48) - 1)

#define GROWTH_FACTOR 1.5f

static inline void fail(const char* error_msg, ...) {
    char buffer[1024];
    va_list args;
    va_start(args, error_msg);
    vsprintf(buffer, error_msg, args);
    va_end(args);
    perror(buffer);
    exit(EXIT_FAILURE);
}

static inline uint_least64_t get_masked(uint_least64_t number,
    uint_least64_t mask)
{
    return number & mask;
}

static inline uint_least64_t set_masked(uint_least64_t number,
    uint_least64_t value, uint_least64_t mask)
{
    return (number & ~mask) + (value & mask);
}

static inline size_t min(size_t a, size_t b) {
    return (a < b) ? a : b;
}

static inline size_t max(size_t a, size_t b) {
    return (a > b) ? a : b;
}

#endif // GLOBAL_H

#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdint.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t bool_t;
typedef size_t Index;

#define TRUE 1
#define FALSE 0

#define NODE_ARRAY_CAPACITY ((uint_least64_t)(1<<10))
#define SPINE_ARRAY_CAPACITY ((uint_least64_t)(1<<10))

#define GROWTH_FACTOR 1.5f

// Control space usage during pretty-printing trees:
// tt(tt(tt)t) vs. t t (t t (t t) t)
#define USE_SPACES

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

#endif // GLOBAL_H

#ifndef GLOBAL_H
#define GLOBAL_H

#include <stdint.h>
#include <assert.h>

typedef uint8_t bool_t;
#define TRUE 1
#define FALSE 0

#define NODE_SEGMENT_SIZE 65536 // 1024
#define FREELIST_SEGMENT_SIZE 65536 // 256
#define SPINE_SEGMENT_SIZE 65536 // 256

// Control space usage during pretty-printing trees:
// tt(tt(tt)t) vs. t t (t t (t t) t)
// #define USE_SPACES

// #define OUTPUT_DIAGRAMS

// Since all nodes consist of two machine words, at least one bit is always
// available for tags

// Tag mask for a single bit (bit 0)
#define TAG_MASK 1U

// Return a tagged version of a pointer-sized value, replacing previous tag
static inline uintptr_t tag_value(uintptr_t value, uint8_t tag) {
    assert((tag & ~TAG_MASK) == 0);
    return (value & ~(uintptr_t)TAG_MASK) | (tag & TAG_MASK);
}

// Return the original value with tag cleared
static inline uintptr_t untag_value(uintptr_t value) {
    return value & ~(uintptr_t)TAG_MASK;
}

// Extract the tag bit (returns 0 or 1)
static inline uint8_t tag_of_value(uintptr_t value) {
    return (uint8_t)(value & TAG_MASK);
}

#endif // GLOBAL_H

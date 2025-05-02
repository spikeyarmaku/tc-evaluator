#ifndef NODE_H
#define NODE_H

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

#include "global.h"

// The tree is stored as a series of Apps. So FLL would be stored as
//     A
//    / \
//   A   L
//  / \
// L   L

// A node is a 128-bit integer value. The first 64 bits point to the first
// child, and the other 64 bits point to the second child. If a pointer is
// NULL, that child is a leaf, otherwise it is an application. The first half
// uses a tag bit. (If there is a need for up to 16 different values, the second
// half can be used as well.)

// The node stack consists of nodes.
// There are also two distinct markers, one is pointing to the first empty
// node, and the other to the last. This is to manage free space efficiently.
// Whenever a node is deleted, the last empty space (pointed to by the end
// marker) is updated to point to the newly deleted node, and then the end
// marker is updated to point to this node as well. That way, eventually each
// empty node would point to the next one, forming a singly linked list. Adding
// and removing elements to it are constant operations.

// Nodes can be:
// - App
//      These contain references to two other nodes (this is the most frequent
//      type)
// - Indirection
//      They contain only one reference to a node, and a refcount to that node.
//      It is used for duplicated nodes
// - Number
//      They contain a 126-bit natural number, that can be incrementally
//      unpacked into a tree
// - System call
//      Contain information regarding a system call

// ------------------------------------ NODE ----------------------------------
// The least significant 2 bits of `left` is a tag:
// 0 - regular pointer to two children
// 1 - reference counted pointer, `left` containing the ref count, `right`
//     containing the pointer to the child
// 2 - a tree encoded as a 126-bit unsigned integer
// 3 - system call
struct Node {
    size_t left;
    size_t right;
};

// Abuse the fact that these pointers are at least 4 bytes long, so the 2 least
// significant bits are always 0. These tags are stored there.
// TODO At this point only App and Indir are considered. Need to incorporate
// the other types.
enum Tag {
    App     = 0,
    
    // An Indir will only appear on the right side of a parent App (due to only
    // rule #2 featuring any copying, and only on the right side of Apps)
    Indir   = 1,
    
    // Number  = 2, // TODO Handle it
    
    // Syscall = 3  // TODO Handle it
};

enum Tag    get_tag (struct Node* node);
void        set_tag (struct Node* node, enum Tag tag);

struct Node*    get_left                (struct Node* node);
struct Node*    get_right               (struct Node* node);
struct Node**   get_left_addr           (struct Node* node);
struct Node**   get_right_addr          (struct Node* node);
struct Node**   get_indir_child_addr    (struct Node* node);
struct Node*    get_indir_child         (struct Node* node);

void    set_left        (struct Node* node, struct Node* left);
void    set_right       (struct Node* node, struct Node* right);
void    set_left_right  (struct Node* node, struct Node* left,
    struct Node* right);
BOOL    is_leaf         (struct Node* node);

void            set_indir_to    (struct Node* node, struct Node* new_node);
BOOL            is_zero_ref     (struct Node* node);
void            incr_ref        (struct Node* node);
void            decr_ref        (struct Node* node);
struct Node*    unset_indir     (struct Node* node);

#endif
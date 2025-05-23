#ifndef TREE_H
#define TREE_H

#include <stdlib.h>
#include <assert.h>

#include "global.h"
#include "node.h"
#include "stack.h"

#include <string.h>

struct Tree {
    struct Node* root;

    // Nodes of the tree
    struct Stack* nodes;

    // A collection of nodes that can be reused
    struct Stack* freelist;
};

struct Tree* tree_make();

// Allocate space for a node
struct Node* alloc_node(struct Tree* tree);
// Add a node to a free empty node
struct Node* add_node(struct Tree* tree, const struct Node* left,
    const struct Node* right);

// Create an indirection node
void duplicate_node_to(struct Tree* tree, struct Node* old_addr,
    struct Node** new_addr);

// Free spaces are stored in a linked list, where every entry points to a tree
// to be deleted later as its left child, and the next free entry as its right
// child.
void delete_node(struct Tree* tree, struct Node* node_to_delete);

struct Node* reparent(struct Node** old_addr, struct Node** new_addr);

void print_empty(int ind, struct Tree* tree);
void print_root(int ind, struct Node* tree);
// Print a textual representation of a tree (e.g. "ttt(tt)")
void pretty_print(struct Node* root);
void print_tree(int ind, struct Tree* tree);

void draw_tree(char* filename, struct Tree* tree);

#endif
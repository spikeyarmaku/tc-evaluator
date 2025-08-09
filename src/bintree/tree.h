#ifndef TREE_H
#define TREE_H

#include <stdlib.h>
#include <assert.h>

#include "global.h"
#include "node.h"
#include "buffer.h"

#include <string.h>

struct Tree {
    // Nodes of the tree
    struct Buffer nodes;
};

struct Tree tree_make   ();

// Add a node to a free empty node
void        add_node    (struct Tree* tree, Index left, Index right);

// Create an indirection node
void duplicate_node_to(struct Tree* tree, struct Node* old_addr,
    struct Node** new_addr);

// Free spaces are stored in a linked list, where every entry points to a tree
// to be deleted later as its left child, and the next free entry as its right
// child.
void delete_node(struct Tree* tree, struct Node* node_to_delete);

void print_empty(int ind, struct Tree* tree);
void print_root(int ind, struct Node* tree);
// Print a textual representation of a tree (e.g. "ttt(tt)")
void pretty_print(struct Node* root);
void print_tree(int ind, struct Tree* tree);

void draw_tree(char* filename, struct Tree* tree);

#endif
#ifndef REDUCTION_H
#define REDUCTION_H

#include "debug.h"
#include "tree.h"

// Return value of reduce(), containing information on the current depth and
// any reducible expressions
struct Reduct {
    int up_depth;
    // The node that is exactly 3 layers above the bottom left node
    struct Node** root; // the address of apps[2], which is the topmost app
    struct Node* apps[3]; // apps[0] is the bottommost app, [1] is its parent
};

void print_reduct(int ind, struct Reduct reduct);

// Apply reduction rules to a subtree
void apply_rules(int ind, struct Tree* tree, struct Reduct reduct);

// Traverse and reduce the tree
// NOTE reduce takes the address of the pointer to the current node because it
// might need to replace the pointer itself to a new node (i.e. with rule #2)
struct Reduct reduce(int ind, struct Tree* tree, struct Node** node_addr,
    size_t down_depth);

#endif
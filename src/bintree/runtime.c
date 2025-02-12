#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

// #define DEBUG_PRINTS
#ifdef DEBUG_PRINTS
#include <stdarg.h>
#endif

void debug(const char* s, ...) {
    #ifdef DEBUG_PRINTS
    va_list args;
    va_start(args, s);
    vprintf(s, args);
    va_end(args);
    #endif
}

void debug_indent(int indent_amount, const char* s, ...) {
    #ifdef DEBUG_PRINTS
    for (int i = 0; i < indent_amount; i++) {
        printf(" ");
    }    
    va_list args;
    va_start(args, s);
    vprintf(s, args);
    va_end(args);
    #endif
}

#define TRUE 1
#define FALSE 0
#define BOOL int

// Abuse the fact that on x86 pointers are aligned, so the 2-3 least
// significant bits are always 0. These tags are stored there
// TODO At this point only App and Indir are considered. Need to incorporate
// the other types.
enum Tag {
    App     = 0
    // Indir   = 1, // TODO Handle it
    // Number  = 2, // TODO Handle it
    // Syscall = 3  // TODO Handle it
};

#define STACK_SEGMENT_SIZE 1024

// ----------------------------- FORWARD DECLS --------------------------------

// This is going to be defined by the transpiler
struct Node* init_program();

struct Node* first_deleted_node = NULL;
struct Node* last_deleted_node  = NULL;
struct Node* global_root        = NULL;
// ----------------------------- /FORWARD DECLS -------------------------------

// The tree is stored as a series of Apps. So FLL would be stored as
//     A
//    / \
//   A   L
//  / \
// L   L

// A node is a 128-bit integer value. The first 64 bits point to the first
// child, and the other 64 bits point to the second child. If a pointer is
// NULL, that child is a leaf, otherwise it is an application. The first half
// uses a tag bit.

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

// Get the lowest 2 bits of the node
enum Tag get_tag(struct Node* node) {
    assert(node != NULL);
    return (node->left & 3);
}

// Set the lowest 2 bits of the node to the provided tag
void set_tag(struct Node* node, enum Tag tag) {
    node->left = !(!node->left | 3) + tag;
}

// TODO Handle numbers as well
struct Node* get_left(struct Node* node) {
    // NOTE: No masking needed here
    // When the node is a regular node, the left tag is 0, so no need to
    // mask off the lowest two bits. In any other situation, the left child is
    // not treated as a pointer, so masking is not needed in that case either.
    return (struct Node*)node->left;
}

struct Node* get_right(struct Node* node) {
    return (struct Node*)node->right;
}

struct Node** get_left_addr(struct Node* node) {
    return (struct Node**)&(node->left);
}

struct Node** get_right_addr(struct Node* node) {
    return (struct Node**)&(node->right);
}

void set_left(struct Node* node, struct Node* left) {
    node->left = (size_t)left;
}

void set_right(struct Node* node, struct Node* right) {
    node->right = (size_t)right;
}

void set_left_right(struct Node* node, struct Node* left, struct Node* right) {
    node->left = (size_t)left;
    node->right = (size_t)right;
}

// TODO Handle syscalls as well
BOOL is_leaf(struct Node* node) {
    if (node == NULL) {
        return TRUE;
    }
    // if (get_tag(node) == Indir) {
    //     return is_leaf(get_right(node));
    // }
    return FALSE;
}

// Mark this app node as a ref-counted indirection to another app node
// void set_indir_to(struct Node* node, struct Node* new_node) {
//     node->right = (size_t)new_node;
//     set_tag(node, Indir);
// }

// BOOL is_zero_ref(struct Node* node) {
//     assert(get_tag(node) == Indir);
//     if ((node->left) == 1) {
//         return TRUE;
//     }
//     return FALSE;
// }

// void incr_ref(struct Node* node) {
//     assert(get_tag(node) == Indir);
//     node->left += 4;
// }

// void decr_ref(struct Node* node) {
//     assert(get_tag(node) == Indir);
//     node->left -= 4;
// }
// ----------------------------------- /NODE ----------------------------------

// ----------------------------- STACK SEGMENT --------------------------------
struct StackSegment {
    struct Node* next_free_addr;
    struct Node* last_free_addr;
    struct StackSegment* next_segment;
    struct StackSegment* prev_segment;
    struct Node nodes[STACK_SEGMENT_SIZE];
};

struct StackSegment* current_segment = NULL;

struct StackSegment* make_segment() {
    struct StackSegment* segment = malloc(sizeof(struct StackSegment));
    segment->next_free_addr = segment->nodes;
    segment->last_free_addr = segment->nodes + STACK_SEGMENT_SIZE - 1;
    segment->next_segment = NULL;
    segment->prev_segment = NULL;
    return segment;

}
void init_stack_segment() {
    current_segment = make_segment();
}

void insert_new_segment() {
    struct StackSegment* segment = make_segment();
    current_segment->next_segment = segment;
    segment->prev_segment = current_segment;
}

void incr_stack() {
    if (current_segment->next_free_addr == current_segment->last_free_addr) {
        if (current_segment->next_segment == NULL) {
            insert_new_segment();
        }
        current_segment = current_segment->next_segment;
    } else {
        current_segment->next_free_addr++;
    }
}

struct Node* grow_stack(struct Node* left, struct Node* right) {
    struct Node* result = current_segment->next_free_addr;
    incr_stack();
    set_left_right(result, left, right);
    return result;
}

// ---------------------------- /STACK SEGMENT --------------------------------

// -------------------------------- NODES -------------------------------------
// Add a node to a free empty node
struct Node* add_node(struct Node* left, struct Node* right) {
    // Check if there are deleted nodes available
    if (first_deleted_node == NULL) {
        // If there are no deleted nodes, add the new node as a new entry
        return grow_stack(left, right);
    } else {
        // Otherwise, check if the first free node has no left children
        if (get_left(first_deleted_node) != NULL) {
            // If it has, move the left child to a new node
            if (get_left(last_deleted_node) == NULL) {
                // (If first_deleted_node is not NULL, last_deleted_node is
                // also not NULL)
                set_left(last_deleted_node, get_left(first_deleted_node));
            } else {
                set_right(last_deleted_node,
                    grow_stack(get_left(first_deleted_node), NULL));
            }
        }
        // Replace this node with the new contents and move
        struct Node* result = first_deleted_node;
        first_deleted_node = get_right(first_deleted_node);
        set_left_right(result, left, right);
        return result;
    }
}

// Create an indirection node
struct Node* duplicate_node(struct Node* node) {
    // struct Node* result = add_node(NULL, node);
    // set_indir_to(result, node);
    // incr_ref(node);
    // return result;

    if (node != NULL) {
        return add_node(
            duplicate_node(get_left(node)), duplicate_node(get_right(node)));
    }
    return node;
}

// Free spaces are stored in a linked list, where every entry points to a tree
// to be deleted later as its left child, and the next free entry as its right
// child.
void delete_node(struct Node* node_to_delete) {
    if (is_leaf(node_to_delete) == TRUE) {
        // Node is already just a node, nothing to do
        return;
    }
    // If it is an indirection, decrease the refcount. If refcount reaches 0,
    // replace it with its right child, and mark the original right child as
    // empty node
    // if (get_tag(node_to_delete) == Indir) {
    //     if (is_zero_ref(node_to_delete) == TRUE) {
    //         struct Node* new_empty_node = get_right(node_to_delete);
    //         set_left(node_to_delete, get_left(new_empty_node));
    //         set_right(node_to_delete, get_right(new_empty_node));
    //         set_left_right(new_empty_node, NULL, NULL);
    //         node_to_delete = new_empty_node;
    //     } else {
    //         decr_ref(node_to_delete);
    //         return;
    //     }
    // }

    // Delete the node, and add its children to the list of empty spaces if
    // necessary
    if (last_deleted_node == NULL) {
        // FIXME This won't work well if the node is Nat or Syscall
        first_deleted_node = node_to_delete;
    }

    if (get_right(node_to_delete) == NULL) {
        // When deleting a tree, add its right child as a new entry, to
        // maintain the invariant that the last entry's right child should
        // be NULL
        struct Node* new_node = grow_stack(get_right(node_to_delete), NULL);
        set_right(node_to_delete, new_node);
        last_deleted_node = new_node;
    } else {
        last_deleted_node = node_to_delete;
    }
}

// Make `root` point to the children of `subtree`, and mark `subtree` as empty
// space. This deletes the `subtree` node itself, but not its content
// void replace_and_delete(struct Node** root, struct Node* subtree) {
//     printf("root: %lu, subtree: %lu\n", (size_t)*root, (size_t)subtree);
//     if (subtree == NULL) {
//         delete_node(*root);
//         *root = NULL;
//     } else {
//         set_left_right(*root, get_left(subtree), get_right(subtree));
//         set_left_right(subtree, NULL, NULL);
//         delete_node(subtree);
//     }
// }

struct Node* detach_left(struct Node* node) {
    assert(node != NULL);
    assert(get_tag(node) == App);

    struct Node* result = get_left(node);
    set_left(node, NULL);
    return result;
}

struct Node* detach_right(struct Node* node) {
    assert(node != NULL);
    assert(get_tag(node) == App);

    struct Node* result = get_right(node);
    set_right(node, NULL);
    return result;
}

void delete_left(struct Node* node) {
    delete_node(detach_left(node));
}

void delete_right(struct Node* node) {
    delete_node(detach_right(node));
}

// -------------------------------- /NODES ------------------------------------

// ------------------------------- DEBUG PRINT --------------------------------

void print_segment(int ind, struct StackSegment* segment) {
    struct Node* node = segment->nodes;

    int node_counter = 0;
    while (node != segment->next_free_addr) {
        debug_indent(ind, "Node #%4d Addr. %14lu: %14lu %14lu\n", node_counter,
            (size_t)&(segment->nodes[node_counter]),
            (size_t)segment->nodes[node_counter].left,
            (size_t)segment->nodes[node_counter].right);
        node_counter++;
        node++;
    }
}

void print_stack(int ind) {
    struct StackSegment* segment = current_segment;
    while (segment->prev_segment != NULL) {
        segment = segment->prev_segment;
    }

    int segment_count = 0;
    do {
        debug_indent(ind, "\nSEGMENT %2d\n----------\n", segment_count);
        print_segment(ind, segment);
        segment = segment->next_segment;
        segment_count++;
    } while (segment != NULL);
}

void print_empty(int ind) {
    if (first_deleted_node == NULL) {
        debug_indent(ind, "No free nodes.\n");
    } else {
        debug_indent(ind, "Free nodes:\n");
        struct Node* current_free_node = first_deleted_node;
        while (current_free_node != NULL) {
            debug_indent(ind, "%lu (%lu)\n", (size_t)current_free_node,
                current_free_node->left);
            current_free_node = get_right(current_free_node);
        }
    }
}

void print_root(int ind, struct Node* tree) {
    debug_indent(ind, "Eval root: %lu\n", (size_t)tree);
}

void print_tree(int ind, struct Node* tree) {
    print_stack(ind);
    printf("\n");
    print_empty(ind);
    printf("\n");
    print_root(ind, tree);
}

void pretty_print_subtree(struct Node* node) {
    if (node == NULL) {
        printf("t");
        return;
    }

    // if (get_tag(node) == Indir) {
    //     pretty_print_subtree(get_right(node));
    //     return;
    // }

    pretty_print_subtree(get_left(node));
    
    if (!is_leaf(get_right(node))) {
        printf("(");
    }
    pretty_print_subtree(get_right(node));
    if (!is_leaf(get_right(node))) {
        printf(")");
    }
}

// Print a textual representation of a tree (e.g. "ttt(tt)")
void pretty_print(struct Node* root) {
    pretty_print_subtree(root);
    printf("\n");
}

// ------------------------------- /DEBUG PRINT -------------------------------

// --------------------------------- REDUCTION --------------------------------
// Return value of reduce(), containing information on the current depth and
// any reducible expressions
struct Reduct {
    int up_depth;
    // The node that is exactly 3 layers above the bottom left node
    struct Node** root;
    struct Node* apps[3]; // apps[0] is the bottommost app, [1] is its parent
};

void print_reduct(int ind, struct Reduct reduct) {
    if (reduct.root == NULL) {
        debug_indent(ind, "Reduct:\n");
        debug_indent(ind, "up_depth: %d\n", reduct.up_depth);
        debug_indent(ind, "root: (NO ROOT)\n");
        debug_indent(ind, "children:\n");
    } else {
        debug_indent(ind, "Reduct:\n");
        debug_indent(ind, "up_depth: %d\n", reduct.up_depth);
        debug_indent(ind, "root: %lu\n", (size_t)*reduct.root);
        debug_indent(ind, "children:\n");
    }
    for (int i = 0; i < 3; i++) {
        debug_indent(ind, "%d: %lu\n", i, (size_t)reduct.apps[i]);
    }
}

struct Reduct make_reduct() {
    struct Reduct reduct;
    reduct.up_depth = 0;
    reduct.root = NULL;
    for (int i = 0; i < 3; i++) {
        reduct.apps[i] = NULL;
    }
    return reduct;
}

struct Reduct reduce(struct Node**, size_t);

// Apply reduction rules to a subtree
// TODO down_depth is not needed here, it is just there for indentation
void apply_rules(struct Reduct reduct, int down_depth) {
    // DEBUG PRINTS
    print_reduct(down_depth * 2, reduct);
    debug_indent(down_depth * 2, "root: %lu\n", (size_t)*reduct.root);

    // Reduce first child
    struct Node* child0 = get_right(reduct.apps[0]);
    if (child0 != NULL) {
        (void)reduce(get_right_addr(reduct.apps[0]), down_depth);
        child0 = get_right(reduct.apps[0]);
    }

    if (is_leaf(child0)) {
        debug_indent(down_depth * 2, "Invoking rule #1: ttab -> a\n");
        // ttab -> a     (first child is a leaf)
        struct Node* a_node = detach_right(reduct.apps[1]);
        delete_node(reduct.apps[2]);
        *reduct.root = a_node;
    } else {
        if (is_leaf(get_left(child0)) == TRUE) {
            debug_indent(down_depth * 2,
                "Invoking rule #2: t(ta)bc -> ac(bc)\n");
            // t(ta)bc -> ac(bc)     (first child is a stem)
            // Delete an App
            struct Node* a_node = detach_right(child0);
            struct Node* b_node = get_right(reduct.apps[1]);
            struct Node* c_node_1 = get_right(reduct.apps[2]);
            struct Node* c_node_2 = duplicate_node(c_node_1);
            delete_right(reduct.apps[0]);
            // Reorganize the other App nodes
            set_right(reduct.apps[2], reduct.apps[0]);
            set_left(reduct.apps[1], a_node);
            set_right(reduct.apps[1], c_node_1);
            set_left(reduct.apps[0], b_node);
            set_right(reduct.apps[0], c_node_2);
        } else {
            // first child is a fork

            // Reduce last child
            struct Node* child2 = get_right(reduct.apps[2]);
            if (child2 != NULL) {
                (void)reduce(get_right_addr(reduct.apps[2]), down_depth);
                child2 = get_right(reduct.apps[2]);
            }

            if (is_leaf(child2)) {
                debug_indent(down_depth * 2,
                    "Invoking rule #3: t(tab)ct -> a\n");
                // t(tab)ct -> a
                struct Node* a_node = detach_right(get_left(child0));
                delete_node(reduct.apps[2]);
                *reduct.root = a_node;
            } else {
                if (is_leaf(get_left(child2))) {
                    debug_indent(down_depth * 2,
                        "Invoking rule #4: t(tab)c(tu) -> bu\n");
                    // t(tab)c(tu) -> bu
                    struct Node* app = detach_right(reduct.apps[2]);
                    struct Node* b_node = detach_right(child0);
                    delete_node(reduct.apps[2]);
                    set_left(app, b_node);
                    *reduct.root = app;
                } else {
                    if (is_leaf(get_left(get_left(child2)))) {
                        debug_indent(down_depth * 2,
                            "Invoking rule #5: t(tab)c(tuv) -> cuv\n");
                        // t(tab)c(tuv) -> cuv
                        struct Node* app = detach_right(reduct.apps[2]);
                        struct Node* c_node = detach_right(reduct.apps[1]);
                        delete_node(reduct.apps[2]);
                        set_left(get_left(app), c_node);
                        *reduct.root = app;
                    } else {
                        // PANIC
                        exit(1);
                    }
                }
            }
        }
    }
}

// Traverse and reduce the tree
struct Reduct reduce(struct Node** node_addr, size_t down_depth) {
    assert(node_addr != NULL);
    // assert(*node_addr != NULL);
    if (*node_addr == NULL) {
        struct Reduct reduct = make_reduct();
        reduct.up_depth = -1;
        return reduct;
    }

    debug_indent(down_depth * 2, "Reducing node type: %d\n",
        get_tag(*node_addr));

    switch (get_tag(*node_addr)) {
        case App: {
            // Check if the left child is a node. If it is, start traversing
            // up the tree, because that is the root of the original tree
            if (is_leaf(get_left(*node_addr))) {
                debug_indent(down_depth * 2, "Found root node\n");
                // Reduction can take place here
                
                // The first child of a node always has to be reduced
                reduce(get_right_addr(*node_addr), 0);
                
                struct Reduct reduct = make_reduct();
                reduct.apps[0] = *node_addr;
                return reduct;
            }

            // Else, continue traversing on the left child
            struct Reduct reduct =
                reduce(get_left_addr(*node_addr), down_depth + 1);
            debug_indent(down_depth * 2,
                "Nothing to reduce, up_depth = %d\n", reduct.up_depth);
            print_reduct(down_depth * 2, reduct);
            reduct.up_depth++;
            reduct.apps[reduct.up_depth] = *node_addr;
            if (reduct.up_depth + down_depth < 2) {
                debug_indent(down_depth * 2,
                    "down_depth + up_depth = %d + %d\n", down_depth,
                    reduct.up_depth);
                return reduce(get_right_addr(*node_addr), 0);
            } else {
                if (reduct.up_depth < 2) {
                    return reduct;
                } else {
                    reduct.root = node_addr;
                    debug_indent(down_depth * 2, "depth = 2, reducing\n");
                    apply_rules(reduct, down_depth);
                    return reduce(node_addr, down_depth);
                }
            }
            break;
        }
        // case Indir: {
        //     return reduce(get_right(node), down_depth);
        //     break;
        // }
        // case Number: {
        //     break;
        // }
        // case Syscall: {
        //     break;
        // }
        default: {
            break;
        }
    }
}
// --------------------------------- /REDUCTION -------------------------------

int main() {
    printf("Tree calculus runtime build 4\n");
    printf("Initializing stack segment\n");
    init_stack_segment();
    printf("Initializing program\n");
    global_root = init_program();
    print_tree(0, global_root);
    pretty_print(global_root);

    printf("Starting reduction\n");
    struct Reduct reduct = reduce(&global_root, 0);
    print_reduct(0, reduct);

    // ----- DEBUG -----
    print_tree(0, global_root);
    pretty_print(global_root);
    return 0;
}

// ------------------------- COMPILER GENERATED CODE: -------------------------


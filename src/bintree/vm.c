// Idea:
// What if init_program consists of just binary code for instructions on how to
// build the tree, and the runtime reduces the tree as it is building it?
// so e.g. it gets the instructions AAALLLL, and by the second L, it realizes
// that rule 1 has to be invoked

// TODO Can there ever be a NULL node that is behind an indirection node?

// TODO Why does this runtime eat 2 gigs of RAM on fib 28? (Probably due to the
// conservative deletion of nodes - indirs are only deleted in `vm_step`)

// NOTE On the TC playground, there are DAG notations for trees. But from my
// tests, reducing a tree in full form vs in DAG form does not differ much in
// reduction speed. DAG form may be faster by a few percentages (Fib 28: 2m3s vs
// 1m59s). Memory consumption is also roughly the same.

// TODO If there are no shared nodes in a rule, repurposing App nodes should be
// fine

#include "vm.h"

#include <inttypes.h>

static void     _apply_rules        (struct VM* vm, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child);

static void     _apply_rule_1       (struct Tree* tree, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index);
static void     _apply_rule_2       (struct Tree* tree, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index);
static void     _apply_rule_3a      (struct Tree* tree, Index top_index,
    Node top_node, Index left_child_index, Node left_child);
static void     _apply_rule_3b      (struct Tree* tree, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child);
static void     _apply_rule_3c      (struct Tree* tree, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child);

static void     _spine_print        (struct Array spine);

// -----------------------------------------------------------------------------

struct VM vm_make(struct Tree tree) {
    struct VM vm;
    vm.tree = tree;
    vm.spine = spine_array_make();
    
    // If the top of the tree is an App, add it to the spine
    Node top = tree_get_node(tree, 0);
    if (node_get_tag(top) == App) {
        spine_array_push(&vm.spine, 0);
    }
    return vm;
}

// Reduction rules:
// A   L         u   -> Su          // Leaf -> Stem
// A   S a       u   -> Fau         // Stem -> Fork
// ---
// A   F L   c   u   -> c           // Rule 1
// A   F Sa  c   u   -> AAauAcu     // Rule 2
// A   F Fab c   L   -> a           // Rule 3a
// A   F Fab c   Su  -> Abu         // Rule 3b
// A   F Fab c   Fuv -> AAcuv       // Rule 3c

// sample eval:  (XX means empty cell, [...] is the currently evaluated node)
// Every entry is a letter - number pair, where the letter is the tag, and the
// number is the index of its first child
// memory                                  matched pattern
//[A1] A3  L0  A5  L0  F7  L0  L0  L0       A A
// A1 [A3] L0  A5  L0  F7  L0  L0  L0       A A
// A1  A3  L0 [A5] L0  F7  L0  L0  L0       A F L c u         --- match!
// A1  A3  L0  L0  L0  XX  XX  XX  XX       [reduction]
// A1 [A3] L0  L0  L0  XX  XX  XX  XX       A L
//[A1] A3  L0  L0  L0  XX  XX  XX  XX       A L
// A1  A3  L0  L0  L0  XX  XX  XX  XX       [end of eval]
enum StepState vm_step(struct VM* vm) {
    // Pop the top of the stack
    bool_t empty = FALSE;
    Index top_index = spine_array_peek(&vm->spine, &empty);

    if (empty == TRUE) {
        return Done;
    }

    _spine_print(vm->spine);
    tree_debug_print(vm->tree);

    // Check the current node. If it is not an App, we're done
    Node top_node = tree_get_node(vm->tree, top_index);
    enum NodeTag top_tag = node_get_tag(top_node);
    if (top_tag != App) {
        if (top_tag == Indirection) {
            spine_array_pop(&vm->spine, &empty);
            return Running;
        } else {
            fail("PANIC! vm_step: invalid top tag: %d\n", top_tag);
        }
    }

    // Check the right child. If it is an App, reduce it
    Index right_child_index = node_get_right_child_index(top_node);
    Node right_child = 0;
    if (right_child_index != 0) {
        right_child = tree_get_node(vm->tree, right_child_index);
        switch (node_get_tag(right_child)) {
            case App: {
                spine_array_push(&vm->spine, right_child_index);
                return Running;
                break;
            }
            case Indirection: {
                debug("Invoking indirection rule (right-side)\n");
                Index indir_index = node_get_indir(right_child);
                tree_decr_refcount(&vm->tree, right_child_index);
                node_set_right_child_index(&top_node, indir_index);
                tree_set_node(&vm->tree, top_index, top_node);

                _spine_print(vm->spine);
                tree_debug_print(vm->tree);
                return Running;
                break;
            }
            default: {
                break;
            }
        }
    }

    // Get the left child and apply the reduction rules to the two children
    Index left_child_index = node_get_left_child_index(top_node);
    if (left_child_index == 0) {
        debug("Invoking rule 0a\n");
        // Left child is a leaf - make a stem
        node_set_tag(&top_node, Stem);
        node_set_left_child_index(&top_node, right_child_index);
        node_set_right_child_index(&top_node, 0);
        tree_set_node(&vm->tree, top_index, top_node);
        spine_array_pop(&vm->spine, &empty);
    } else {
        Node left_child = tree_get_node(vm->tree, left_child_index);
        switch (node_get_tag(left_child)) {
            case Indirection: {
                debug("Invoking indirection rule (left side)\n");
                Index indir_index = node_get_indir(left_child);
                tree_decr_refcount(&vm->tree, left_child_index);
                node_set_left_child_index(&top_node, indir_index);
                tree_set_node(&vm->tree, top_index, top_node);
                break;
            }
            case Stem: {
                debug("Invoking rule 0b\n");
                // Left child is a stem - make a fork
                tree_decr_refcount(&vm->tree, left_child_index);
                node_set_tag(&top_node, Fork);
                node_set_left_child_index(&top_node,
                    node_get_left_child_index(left_child));
                node_set_right_child_index(&top_node, right_child_index);
                tree_set_node(&vm->tree, top_index, top_node);
                spine_array_pop(&vm->spine, &empty);
                break;
            }
            case Fork: {
                _apply_rules(vm, top_index, top_node, left_child_index,
                    left_child, right_child_index, right_child);
                break;
            }
            case App: {
                spine_array_push(&vm->spine, left_child_index);
                return Running;
                break;
            }
            default: {
                fail("PANIC! Invalid tag during evaluation: %d\n",
                    node_get_tag(left_child));
                break;
            }
        }
    }
    return Running;
}

void vm_run(struct VM* vm) {
    enum StepState state = Running;

    size_t counter = 0;
    while (state == Running) {
        debug("--- STEP %d ---\n", counter++);
        state = vm_step(vm);
    }
}

// Serialize the VM's state into a byte array
void* vm_serialize(struct VM vm, size_t* size) {
    // TODO
}

struct VM vm_deserialize(void* data) {
    // TODO
}

// -------------------------------- INTERNAL METHODS ---------------------------

static void _apply_rules(struct VM* vm, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index,
    Node right_child)
{
    if (node_get_left_child_index(left_child) == 0) {
        // Rule 1
        _apply_rule_1(&vm->tree, top_index, top_node, left_child_index, left_child,
            right_child_index);
    } else {
        Index left_of_left_index = node_get_left_child_index(left_child);
        Node left_of_left_child = tree_get_node(vm->tree, left_of_left_index);
        switch (node_get_tag(left_of_left_child)) {
            case Stem: {
                // Rule 2
                _apply_rule_2(&vm->tree, top_index, top_node, left_child_index,
                    left_child, right_child_index);
                break;
            }
            case Fork: {
                // Rule 3a-3c
                if (right_child_index == 0) {
                    // Rule 3a
                    _apply_rule_3a(&vm->tree, top_index, top_node, left_child_index,
                        left_child);
                } else {
                    switch (node_get_tag(right_child)) {
                        case Stem: {
                            // Rule 3b
                            _apply_rule_3b(&vm->tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        case Fork: {
                            // Rule 3c
                            _apply_rule_3c(&vm->tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        default: {
                            fail("PANIC! Invalid tag during rule application: "
                                "%d\n", node_get_tag(right_child));
                            break;
                        }
                    }
                }
                break;
            }
            default: {
                fail("PANIC! Invalid tag during rule application: %d\n",
                    node_get_tag(left_child));
                break;
            }
        }
    }
}

// Rule 1
// A FLc u -> c
//       A            (I)
//      / \            |
//     F   u    --->   c
//    / \
//   L   c
// Note: since the parent needs to be connected to the child (essentially the
// top node shall be skipped), the top node is converted into an indirection
// node, which will be eliminated during the evaluation
static void _apply_rule_1(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index)
{
    debug("Invoking rule 1\n");
    Index node_c_index = node_get_right_child_index(left_child);
    node_set_indir(&top_node, node_c_index);
    tree_set_node(tree, top_index, top_node);
    tree_incr_refcount(tree, node_c_index);
    tree_decr_refcount(tree, left_child_index);
    tree_decr_refcount(tree, right_child_index);
}

// Rule 2
// A F(Sa)c u -> A(Aau)(Acu)
//        A                A
//       / \              / \
//      F   u            /   \
//     / \       --->   A     A
//    S   c            / \   / )
//    |               a   \ c /
//    a                    \ /
//                          u
static void _apply_rule_2(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index)
{
    debug("Invoking rule 2\n");
    Index node_S_index = node_get_left_child_index(left_child);
    Node node_S = tree_get_node(*tree, node_S_index);
    Index node_a_index = node_get_left_child_index(node_S);
    Index node_c_index = node_get_right_child_index(left_child);
    Index left_app_index = tree_add_node(tree, App, node_a_index,
        right_child_index);
    Index right_app_index = tree_add_node(tree, App, node_c_index,
        right_child_index);
    node_set_left_child_index(&top_node, left_app_index);
    node_set_right_child_index(&top_node, right_app_index);
    tree_set_node(tree, top_index, top_node);
    tree_incr_refcount(tree, node_a_index);
    tree_incr_refcount(tree, node_c_index);
    tree_incr_refcount(tree, node_c_index);
    tree_incr_refcount(tree, right_child_index);
    tree_decr_refcount(tree, left_child_index);
    tree_decr_refcount(tree, right_child_index);
}

// Rule 3a
// A F(Fab)c L -> a
//       A
//      / \         (I)
//     F   L         |
//    / \      --->  a
//   F   c
//  / \
// a   b
// Note: since the parent needs to be connected to the child (essentially the
// top node shall be skipped), the top node is converted into an indirection
// node, which will be eliminated during the evaluation
static void _apply_rule_3a(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child)
{
    debug("Invoking rule 3a\n");
    node_set_tag(&top_node, Indirection);
    Index node_lower_F_index = node_get_left_child_index(left_child);
    Node node_lower_F = tree_get_node(*tree, node_lower_F_index);
    Index node_a_index = node_get_left_child_index(node_lower_F);
    node_set_right_child_index(&top_node, node_a_index);
    tree_set_node(tree, top_index, top_node);
    tree_incr_refcount(tree, node_a_index);
    tree_decr_refcount(tree, left_child_index);
}

// Rule 3b
// A F(Fab)c Su -> Abu
//       A
//      / \
//     F   S           A
//    / \  |   --->   / \
//   F   c u         b   u
//  / \
// a   b  
static void _apply_rule_3b(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index,
    Node right_child)
{
    debug("Invoking rule 3b\n");
    Index node_lower_F_index = node_get_left_child_index(left_child);
    Node node_lower_f = tree_get_node(*tree, node_lower_F_index);
    Index node_b_index = node_get_right_child_index(node_lower_f);
    Index node_u_index = node_get_left_child_index(right_child);
    node_set_left_child_index(&top_node, node_b_index);
    node_set_right_child_index(&top_node, node_u_index);
    tree_set_node(tree, top_index, top_node);
    tree_incr_refcount(tree, node_b_index);
    tree_incr_refcount(tree, node_u_index);
    tree_decr_refcount(tree, left_child_index);
    tree_decr_refcount(tree, right_child_index);
}

// Rule 3c
// A F(Fab)c Fuv -> AAcuv
//        A
//       / \
//      /   \             A
//     F     F           / \
//    / \   / \   --->  A   v
//   F   c u   v       / \
//  / \               c   u
// a   b  
static void _apply_rule_3c(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index,
    Node right_child)
{
    debug("Invoking rule 3c\n");
    Index node_c_index = node_get_right_child_index(left_child);
    Index node_u_index = node_get_left_child_index(right_child);
    Index node_v_index = node_get_right_child_index(right_child);
    Index left_app_index = tree_add_node(tree, App, node_c_index, node_u_index);
    node_set_left_child_index(&top_node, left_app_index);
    node_set_right_child_index(&top_node, node_v_index);
    tree_set_node(tree, top_index, top_node);
    tree_incr_refcount(tree, node_c_index);
    tree_incr_refcount(tree, node_u_index);
    tree_incr_refcount(tree, node_v_index);
    tree_decr_refcount(tree, left_child_index);
    tree_decr_refcount(tree, right_child_index);
}

static void _spine_print(struct Array spine) {
    printf("Spine: ");
    size_t count = spine.size / sizeof(Index);
    for (size_t i = 0; i < count; i++) {
        Index index = *(Index*)array_get(spine, i, sizeof(Index));
        printf("%lu ", index);
    }
    printf("\n");
}
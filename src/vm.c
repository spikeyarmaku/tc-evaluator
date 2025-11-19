// TODO If there are no shared nodes in a rule, repurposing App nodes should be
// fine

#include "vm.h"
#include "array.h"
#include "global.h"
#include "node.h"
#include "tree.h"

#include "debug.h"
#include <string.h>

const struct VMHeader vm_default_header = {"TCVM", 1, 0};
const struct VMConfig vm_default_config = {1<<10, 1<<10};

static void     _apply_rules        (struct Tree* tree, Index top_index,
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

struct VM vm_make(struct VMConfig config) {
    struct VM vm;
    vm.tree = tree_make(config.initial_tree_capacity);
    vm.spine = spine_array_make(config.initial_spine_capacity);

    // The first node of the tree is a pointer to the top of the tree
    tree_add_node(&vm.tree, Indirection, 0, 0);
    return vm;
}

void vm_free(struct VM* vm) {
    tree_free(&vm->tree);
    array_free(&vm->spine);
}

void vm_init(struct VM* vm) {
    spine_array_push(&vm->spine, 0);
}

enum StepState vm_step(struct VM* vm) {
    // Pop the top of the stack
    bool_t empty = FALSE;
    Index top_index = spine_array_peek(vm->spine, &empty);
    if (empty == TRUE) {
        return Done;
    }

    // Check the current node. If it is not an App, we're done
    Node top_node = tree_get_node(vm->tree, top_index);
    enum NodeTag top_tag = node_get_tag(top_node);
    if (top_tag != App) {
        if (top_tag == Indirection) {
            if (top_index == 0) {
                // If the top node is an indirection, it points to the root of
                // the tree. Follow it
                spine_array_push(&vm->spine, node_get_indir(top_node));
            } else {
                // There's nothing that can be done when an indirection is the
                // top node. Go back one level and try again
                spine_array_pop(&vm->spine, &empty);
            }
            return Running;
        } else {
            return Done;
        }
    }

    // Check the right child. If it is an App, reduce it
    Index right_child_index = node_get_right_child_index(top_node);
    Node right_child;
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
                // A a Ib -> Aab
                Index indir_index = node_get_indir(right_child);
                tree_decr_refcount(&vm->tree, right_child_index);
                node_set_right_child_index(&top_node, indir_index);
                tree_set_node(&vm->tree, top_index, top_node);
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
        // A L b -> Sb
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
                // A Ia b -> Aab
                Index indir_index = node_get_indir(left_child);
                tree_decr_refcount(&vm->tree, left_child_index);
                node_set_left_child_index(&top_node, indir_index);
                tree_set_node(&vm->tree, top_index, top_node);
                break;
            }
            case Stem: {
                debug("Invoking rule 0b\n");
                // A Sa b -> Fab
                // Left child is a stem - make a fork
                tree_decr_refcount(&vm->tree, left_child_index);
                Index node_a_index = node_get_left_child_index(left_child);
                node_set_tag(&top_node, Fork);
                node_set_left_child_index(&top_node, node_a_index);
                node_set_right_child_index(&top_node, right_child_index);
                tree_incr_refcount(&vm->tree, node_a_index);
                tree_set_node(&vm->tree, top_index, top_node);
                spine_array_pop(&vm->spine, &empty);
                break;
            }
            case Fork: {
                _apply_rules(&vm->tree, top_index, top_node, left_child_index,
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
        debug("--- STEP %lu ---\n", counter++);
        // tree_debug_print(vm->tree);
        // _spine_print(vm->spine);
        state = vm_step(vm);
    }
}

size_t vm_get_size(struct VM vm) {
    return vm.tree.nodes.size;
}

// Serialize the VM's state into a byte array
void vm_serialize(vm_write_fn write_fn, struct VM vm, size_t chunk_size,
    void* ctx)
{
    size_t total_size = vm_get_size(vm);
    size_t written_total = 0;
    struct VMHeader header = vm_default_header;
    header.size = total_size;
    write_fn(ctx, &header, sizeof(struct VMHeader));
    while (total_size > written_total) {
        written_total += write_fn(ctx, vm.tree.nodes.data + written_total,
            min(total_size - written_total, chunk_size));
    }
}

enum VMResult vm_deserialize(struct VM* vm, vm_read_fn read_fn,
    size_t chunk_size, void* ctx)
{
    struct VMHeader header;
    size_t read = read_fn(ctx, &header, sizeof(struct VMHeader));
    if (read != sizeof(struct VMHeader)) {
        return VM_ERR_TRUNCATED;
    }
    if (strncmp(header.magic, vm_default_header.magic, 4) != 0) {
        return VM_ERR_MAGIC_MISMATCH;
    }
    // TODO check version
    // if (vm_check_version(header.version) != 0) {
    // return VM_ERR_UNSUPPORTED_VERSION
    // }
    struct VMConfig config = vm_default_config;
    config.initial_tree_capacity = header.size;
    *vm = vm_make(config);

    size_t total = 0;
    while (total < header.size) {
        total += read_fn(ctx, vm->tree.nodes.data + total,
            min(header.size - total, chunk_size));
    }
    vm->tree.nodes.size = total;
    return VM_OK;
}

// -------------------------------- INTERNAL METHODS ---------------------------

static void _apply_rules(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index,
    Node right_child)
{
    debug("Top index: %lu\n", top_index);
    if (node_get_left_child_index(left_child) == 0) {
        // Rule 1
        _apply_rule_1(tree, top_index, top_node, left_child_index, left_child,
            right_child_index);
    } else {
        Index left_of_left_index = node_get_left_child_index(left_child);
        Node left_of_left_child = tree_get_node(*tree, left_of_left_index);
        switch (node_get_tag(left_of_left_child)) {
            case Stem: {
                // Rule 2
                _apply_rule_2(tree, top_index, top_node, left_child_index,
                    left_child, right_child_index);
                break;
            }
            case Fork: {
                // Rule 3a-3c
                if (right_child_index == 0) {
                    // Rule 3a
                    _apply_rule_3a(tree, top_index, top_node, left_child_index,
                        left_child);
                } else {
                    switch (node_get_tag(right_child)) {
                        case Stem: {
                            // Rule 3b
                            _apply_rule_3b(tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        case Fork: {
                            // Rule 3c
                            _apply_rule_3c(tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        default: {
                            fail("PANIC! Invalid right tag during rule "
                                "application: %d\n", node_get_tag(right_child));
                            break;
                        }
                    }
                }
                break;
            }
            default: {
                fail("PANIC! Invalid left tag during rule application: %d\n",
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
    tree_incr_refcount(tree, right_child_index);
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
    Index node_lower_F_index = node_get_left_child_index(left_child);
    Node node_lower_F = tree_get_node(*tree, node_lower_F_index);
    Index node_a_index = node_get_left_child_index(node_lower_F);
    node_set_indir(&top_node, node_a_index);
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

#include "vm.h"
#include "array.h"
#include "global.h"
#include "node.h"
#include "tree.h"

#include "debug.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

static const char* magic_default = "SHRB";
static const uint8_t version_major = 1;
static const uint8_t version_minor = 0;
const struct VmConfig vm_default_config = {1<<10, 1<<10};

static void     _apply_rules        (struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child);

static void     _apply_rule_1       (struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index);
static void     _apply_rule_2       (struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index);
static void     _apply_rule_3a      (struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child);
static void     _apply_rule_3b      (struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child);
static void     _apply_rule_3c      (struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child);

static void     _vm_spine_rewind     (struct Vm* vm);

// -----------------------------------------------------------------------------

struct Vm vm_make(struct VmConfig config) {
    struct Vm vm;
    vm.tree = tree_make(config.initial_tree_capacity);
    vm.spine = spine_array_make(config.initial_spine_capacity);

    // The first node of the tree is a pointer to the top of the tree
    tree_add_node(&vm.tree, node_make(NODE_TYPE_INDIR, 1, 0, 0));
    spine_array_push(&vm.spine, 0);
    return vm;
}

void vm_free(struct Vm* vm) {
    tree_free(&vm->tree);
    array_free(&vm->spine);
}

enum VmState vm_step(struct Vm* vm) {
    // Pop the top of the stack
    bool_t empty = FALSE;
    Index top_index = spine_array_peek(vm->spine, &empty);
    if (empty == TRUE) {
        spine_array_push(&vm->spine, 0);
        return VM_STATE_DONE;
    }

    // Check the current node. If it is not an App, we're done
    Node top_node = tree_get_node(vm->tree, top_index);
    enum NodeType top_type = node_get_type(top_node);
    if (top_type != NODE_TYPE_APP) {
        if (top_type == NODE_TYPE_INDIR) {
            if (top_index == 0) {
                Index indir_index = node_get_indir(top_node);
                if (indir_index == 0) {
                    _vm_spine_rewind(vm);
                    return VM_STATE_DONE;
                }
                Node indir_node = tree_get_node(vm->tree, indir_index);
                if (node_get_type(indir_node) != NODE_TYPE_INDIR) {
                    // If the top node is an indirection, it points to the root
                    // of the tree. Follow it
                    spine_array_push(&vm->spine, node_get_indir(top_node));
                } else {
                    tree_set_node(vm->tree, top_index,
                        node_set_indir(top_node, node_get_indir(indir_node)));
                }
            } else {
                // There's nothing that can be done when an indirection is the
                // top node. Go back one level and try again
                spine_array_pop(&vm->spine, &empty);
            }
            return VM_STATE_RUNNING;
        } else {
            _vm_spine_rewind(vm);
            return VM_STATE_DONE;
        }
    }

    // Check the right child. If it is an App, reduce it
    Index right_child_index = node_get_child(top_node, CHILD_SIDE_RIGHT);
    Node right_child;
    if (right_child_index != 0) {
        right_child = tree_get_node(vm->tree, right_child_index);
        switch (node_get_type(right_child)) {
            case NODE_TYPE_APP: {
                spine_array_push(&vm->spine, right_child_index);
                return VM_STATE_RUNNING;
                break;
            }
            case NODE_TYPE_INDIR: {
                debug("Invoking indirection rule (right-side)\n");
                // A a Ib -> Aab
                tree_set_node(vm->tree, top_index,
                    node_set_child(top_node, CHILD_SIDE_RIGHT,
                        node_get_indir(
                            tree_get_node(vm->tree, right_child_index))));
                assert(node_get_refcount(right_child) == 1);
                tree_delete_node(&vm->tree, right_child_index);
                return VM_STATE_RUNNING;
                break;
            }
            default: {
                break;
            }
        }
    }

    // Get the left child and apply the reduction rules to the two children
    Index left_child_index = node_get_child(top_node, CHILD_SIDE_LEFT);
    if (left_child_index == 0) {
        debug("Invoking rule 0a\n");
        // A L b -> Sb
        // Left child is a leaf - make a stem
        tree_set_node(vm->tree, top_index,
            node_set_type(top_node, NODE_TYPE_STEM));
        spine_array_pop(&vm->spine, &empty);
    } else {
        Node left_child = tree_get_node(vm->tree, left_child_index);
        switch (node_get_type(left_child)) {
            case NODE_TYPE_INDIR: {
                debug("Invoking indirection rule (left side)\n");
                // A Ia b -> Aab
                tree_set_node(vm->tree, top_index,
                    node_set_child(top_node, CHILD_SIDE_LEFT,
                        node_get_indir(
                            tree_get_node(vm->tree, left_child_index))));
                assert(node_get_refcount(left_child) == 1);
                tree_delete_node(&vm->tree, left_child_index);
                break;
            }
            case NODE_TYPE_STEM: {
                debug("Invoking rule 0b\n");
                // A Sa b -> Fab
                // Left child is a stem - make a fork
                Index node_a_index = node_get_child(left_child, CHILD_SINGLE);
                tree_set_node(vm->tree, top_index,
                    node_set_child(
                        node_set_type(top_node, NODE_TYPE_FORK),
                        CHILD_SIDE_LEFT, node_a_index));
                tree_decr_refcount(&vm->tree, left_child_index);
                tree_incr_refcount(&vm->tree, node_a_index);
                spine_array_pop(&vm->spine, &empty);
                break;
            }
            case NODE_TYPE_FORK: {
                _apply_rules(&vm->tree, top_index, top_node, left_child_index,
                    left_child, right_child_index, right_child);
                break;
            }
            case NODE_TYPE_APP: {
                spine_array_push(&vm->spine, left_child_index);
                return VM_STATE_RUNNING;
                break;
            }
            default: {
                fail("PANIC! Invalid tag during evaluation: %d\n",
                    node_get_type(left_child));
                break;
            }
        }
    }
    return VM_STATE_RUNNING;
}

void vm_run(struct Vm* vm) {
    enum VmState state = VM_STATE_RUNNING;

    size_t counter = 0;
    while (state == VM_STATE_RUNNING) {
        // printf("--- STEP %lu ---\n", counter);
        // tree_debug_print(vm->tree);
        // _spine_print(vm->spine);
        // tree_print_comb(vm->tree);
        state = vm_step(vm);
        counter++;
    }

    debug("%lu steps\n", counter);
}

// void vm_compact(struct Vm* vm) {
    // TODO
    // Move the top non-null nodes of the node array to empty spaces
    // Decrease refcounts of the overwritten node's children
    // Rewrite indices pointing to these nodes in the node array
    // Rewrite indices pointing to these nodes in the spine array
// }

size_t vm_get_size(struct Vm vm) {
    return vm.tree.nodes.size;
}

// Serialize the VM's state into a byte array
void vm_write(struct Vm vm, void* user_data, uint16_t user_data_size,
    size_t chunk_size, vm_write_fn write_fn, void* ctx)
{
    size_t total_size = vm_get_size(vm);
    size_t written_total = 0;
    struct VmHeader header;
    memcpy(header.magic, magic_default, 4);
    header.version_major = version_major;
    header.version_minor = version_minor;
    header.user_data_size = user_data_size;
    header.size = total_size;
    write_fn(ctx, &header, sizeof(struct VmHeader));
    while (user_data_size > written_total) {
        written_total += write_fn(ctx, user_data, user_data_size);
    }
    written_total = 0;
    while (total_size > written_total) {
        written_total += write_fn(ctx, vm.tree.nodes.data + written_total,
            min(total_size - written_total, chunk_size));
    }
}

enum VmResult vm_read_header(struct VmHeader* header_h, size_t chunk_size,
    vm_read_fn read_fn, void* ctx)
{
    size_t read = read_fn(ctx, header_h, sizeof(struct VmHeader));
    if (read != sizeof(struct VmHeader)) {
        return VM_ERR_TRUNCATED;
    }
    if (strncmp(header_h->magic, magic_default, 4) != 0) {
        return VM_ERR_MAGIC_MISMATCH;
    }
    if (header_h->version_major > version_major) {
        return VM_ERR_UNSUPPORTED_VERSION;
    }
    return VM_OK;
}

enum VmResult vm_read_user_data(const struct VmHeader* header_h,
    void* user_data_h, vm_read_fn read_fn, void* ctx)
{
    struct VmHeader header = *header_h;
    // Read user data
    size_t read = read_fn(ctx, user_data_h, header.user_data_size);
    if (read != header.user_data_size) {
        return VM_ERR_TRUNCATED;
    }
    return VM_OK;
}

enum VmResult vm_read_vm_data(const struct VmHeader* header_h, struct Vm* vm_h,
    size_t chunk_size, vm_read_fn read_fn, void* ctx)
{
    struct VmHeader header = *header_h;
    // Read VM data
    struct VmConfig config = vm_default_config;
    config.initial_tree_capacity = header_h->size;
    *vm_h = vm_make(config);
    size_t total = 0;
    size_t read = 0;
    while (total < header.size) {
        size_t needed = min(header.size - total, chunk_size);
        read = read_fn(ctx, vm_h->tree.nodes.data + total, needed);
        if (read != needed) {
            return VM_ERR_TRUNCATED;
        }
        total += read;
    }
    vm_h->tree.nodes.size = total;
    return VM_OK;
}

// -------------------------------- INTERNAL METHODS ---------------------------

static void _apply_rules(struct Tree* tree, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index,
    Node right_child)
{
    debug("Top index: %lu\n", top_index);
    if (node_get_child(left_child, CHILD_SIDE_LEFT) == 0) {
        // Rule 1
        _apply_rule_1(tree, top_index, top_node, left_child_index, left_child,
            right_child_index);
    } else {
        Index left_of_left_index = node_get_child(left_child,
            CHILD_SIDE_LEFT);
        Node left_of_left_child = tree_get_node(*tree, left_of_left_index);
        switch (node_get_type(left_of_left_child)) {
            case NODE_TYPE_STEM: {
                // Rule 2
                _apply_rule_2(tree, top_index, top_node, left_child_index,
                    left_child, right_child_index);
                break;
            }
            case NODE_TYPE_FORK: {
                // Rule 3a-3c
                if (right_child_index == 0) {
                    // Rule 3a
                    _apply_rule_3a(tree, top_index, top_node, left_child_index,
                        left_child);
                } else {
                    switch (node_get_type(right_child)) {
                        case NODE_TYPE_STEM: {
                            // Rule 3b
                            _apply_rule_3b(tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        case NODE_TYPE_FORK: {
                            // Rule 3c
                            _apply_rule_3c(tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        default: {
                            fail("PANIC! Invalid right tag during rule "
                                "application: %d\n",
                                node_get_type(right_child));
                            break;
                        }
                    }
                }
                break;
            }
            default: {
                fail("PANIC! Invalid left tag during rule application: %d\n",
                    node_get_type(left_child));
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
static void _apply_rule_1(struct Tree* tree_h, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index)
{
    debug("Invoking rule 1\n");
    struct Tree tree = *tree_h;
    Index node_c_index = node_get_child(left_child, CHILD_SIDE_RIGHT);
    Node node_c = tree_get_node(tree, node_c_index);
    // Change app
    tree_set_node(tree, top_index, node_set_indir(top_node, node_c_index));
    // Incr c's refcount
    tree_set_node(tree, node_c_index, node_incr_refcount(node_c));
    // Decr left child's refcount
    tree_decr_refcount(tree_h, left_child_index);
    // Decr right child's refcount
    tree_decr_refcount(tree_h, right_child_index);
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
static void _apply_rule_2(struct Tree* tree_h, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index)
{
    debug("Invoking rule 2\n");
    struct Tree tree = *tree_h;
    Index node_S_index = node_get_child(left_child, CHILD_SIDE_LEFT);
    Index node_c_index = node_get_child(left_child, CHILD_SIDE_RIGHT);
    Node node_S = tree_get_node(tree, node_S_index);
    Index node_a_index = node_get_child(node_S, CHILD_SINGLE);
    Index left_app_index = tree_add_node(tree_h,
        node_make(NODE_TYPE_APP, 1, node_a_index, right_child_index));
    Index right_app_index = tree_add_node(tree_h,
        node_make(NODE_TYPE_APP, 1, node_c_index, right_child_index));
    tree_set_node(tree, top_index, node_set_child(
        node_set_child(top_node, CHILD_SIDE_LEFT, left_app_index),
        CHILD_SIDE_RIGHT, right_app_index));
    tree_incr_refcount(tree_h, node_a_index);
    tree_incr_refcount(tree_h, node_c_index);
    tree_incr_refcount(tree_h, right_child_index);
    tree_decr_refcount(tree_h, left_child_index);
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
static void _apply_rule_3a(struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child)
{
    debug("Invoking rule 3a\n");
    struct Tree tree = *tree_h;
    Index node_lower_F_index = node_get_child(left_child,
        CHILD_SIDE_LEFT);
    Node node_lower_F = tree_get_node(tree, node_lower_F_index);
    Index node_a_index = node_get_child(node_lower_F, CHILD_SIDE_LEFT);
    tree_set_node(tree, top_index, node_set_indir(top_node, node_a_index));
    tree_incr_refcount(tree_h, node_a_index);
    tree_decr_refcount(tree_h, left_child_index);
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
static void _apply_rule_3b(struct Tree* tree_h, Index top_index,
    Node top_node, Index left_child_index, Node left_child,
    Index right_child_index, Node right_child)
{
    debug("Invoking rule 3b\n");
    struct Tree tree = *tree_h;
    Index node_lower_F_index = node_get_child(left_child,
        CHILD_SIDE_LEFT);
    Node node_lower_F = tree_get_node(tree, node_lower_F_index);
    Index node_b_index = node_get_child(node_lower_F, CHILD_SIDE_RIGHT);
    Index node_u_index = node_get_child(right_child, CHILD_SINGLE);
    tree_set_node(tree, top_index,
        node_set_child(
            node_set_child(top_node, CHILD_SIDE_LEFT, node_b_index),
            CHILD_SIDE_RIGHT, node_u_index));
    tree_decr_refcount(tree_h, left_child_index);
    tree_decr_refcount(tree_h, right_child_index);
    tree_incr_refcount(tree_h, node_b_index);
    tree_incr_refcount(tree_h, node_u_index);
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
static void _apply_rule_3c(struct Tree* tree_h, Index top_index, Node top_node,
    Index left_child_index, Node left_child, Index right_child_index,
    Node right_child)
{
    debug("Invoking rule 3c\n");
    struct Tree tree = *tree_h;
    Index node_c_index = node_get_child(left_child, CHILD_SIDE_RIGHT);
    Index node_u_index = node_get_child(right_child, CHILD_SIDE_LEFT);
    Node left_app = node_make(NODE_TYPE_APP, 1, node_c_index, node_u_index);
    Index left_app_index = tree_add_node(tree_h, left_app);
    Index node_v_index = node_get_child(right_child, CHILD_SIDE_RIGHT);
    tree_set_node(tree, top_index,
        node_set_child(
            node_set_child(top_node, CHILD_SIDE_LEFT, left_app_index),
            CHILD_SIDE_RIGHT, node_v_index));
    tree_decr_refcount(tree_h, left_child_index);
    tree_decr_refcount(tree_h, right_child_index);
    tree_incr_refcount(tree_h, node_c_index);
    tree_incr_refcount(tree_h, node_u_index);
    tree_incr_refcount(tree_h, node_v_index);
}

void vm_spine_print(struct Array spine) {
    printf("Spine: ");
    size_t count = spine.size / sizeof(Index);
    for (size_t i = 0; i < count; i++) {
        Index index = *(Index*)array_get(spine, i, sizeof(Index));
        printf("%lu ", index);
    }
    printf("\n");
}

// --- PRIVATE METHODS ---
static void _vm_spine_rewind(struct Vm* vm) {
    bool_t error = FALSE;
    while (error == FALSE) {
        spine_array_pop(&vm->spine, &error);
    }
    spine_array_push(&vm->spine, 0);
}

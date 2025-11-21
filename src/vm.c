// TODO If there are no shared nodes in a rule, repurposing App nodes should be
// fine

#include "vm.h"
#include "array.h"
#include "global.h"
#include "node.h"
#include "tree.h"

#include "debug.h"
#include <string.h>

const struct VmHeader vm_default_header = {"TCVM", {1, 0}, 0};
const struct VmConfig vm_default_config = {1<<10, 1<<10};

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

struct Vm vm_make(struct VmConfig config) {
    struct Vm vm;
    vm.tree = tree_make(config.initial_tree_capacity);
    vm.spine = spine_array_make(config.initial_spine_capacity);

    // The first node of the tree is a pointer to the top of the tree
    tree_add_node(&vm.tree, NODE_TAG_INDIR);
    return vm;
}

void vm_free(struct Vm* vm) {
    tree_free(&vm->tree);
    array_free(&vm->spine);
}

void vm_init(struct Vm* vm) {
    spine_array_push(&vm->spine, 0);
}

enum VmState vm_step(struct Vm* vm) {
    // Pop the top of the stack
    bool_t empty = FALSE;
    Index top_index = spine_array_peek(vm->spine, &empty);
    if (empty == TRUE) {
        return VM_STATE_DONE;
    }

    // Check the current node. If it is not an App, we're done
    Node top_node = tree_get_node(vm->tree, top_index);
    enum NodeTag top_tag = node_get_tag(top_node);
    if (top_tag != NODE_TAG_APP) {
        if (top_tag == NODE_TAG_INDIR) {
            if (top_index == 0) {
                Index indir_index = node_get_indir(top_node);
                if (indir_index == 0) {
                    return VM_STATE_DONE;
                }
                Node indir_node = tree_get_node(vm->tree, indir_index);
                if (node_get_tag(indir_node) != NODE_TAG_INDIR) {
                    // If the top node is an indirection, it points to the root
                    // of the tree. Follow it
                    spine_array_push(&vm->spine, node_get_indir(top_node));
                } else {
                    tree_change_child(&vm->tree, top_index, CHILD_SIDE_LEFT,
                        node_get_indir(indir_node));
                }
            } else {
                // There's nothing that can be done when an indirection is the
                // top node. Go back one level and try again
                spine_array_pop(&vm->spine, &empty);
            }
            return VM_STATE_RUNNING;
        } else {
            return VM_STATE_DONE;
        }
    }

    // Check the right child. If it is an App, reduce it
    Index right_child_index = node_get_child_index(top_node, CHILD_SIDE_RIGHT);
    Node right_child;
    if (right_child_index != 0) {
        right_child = tree_get_node(vm->tree, right_child_index);
        switch (node_get_tag(right_child)) {
            case NODE_TAG_APP: {
                spine_array_push(&vm->spine, right_child_index);
                return VM_STATE_RUNNING;
                break;
            }
            case NODE_TAG_INDIR: {
                debug("Invoking indirection rule (right-side)\n");
                // A a Ib -> Aab
                tree_move_child(&vm->tree, right_child_index, CHILD_SIDE_LEFT,
                    top_index, CHILD_SIDE_RIGHT);
                return VM_STATE_RUNNING;
                break;
            }
            default: {
                break;
            }
        }
    }

    // Get the left child and apply the reduction rules to the two children
    Index left_child_index = node_get_child_index(top_node, CHILD_SIDE_LEFT);
    if (left_child_index == 0) {
        debug("Invoking rule 0a\n");
        // A L b -> Sb
        // Left child is a leaf - make a stem
        tree_move_child(&vm->tree, top_index, CHILD_SIDE_RIGHT, top_index,
            CHILD_SIDE_LEFT);
        tree_change_tag(&vm->tree, top_index, NODE_TAG_STEM);
        spine_array_pop(&vm->spine, &empty);
    } else {
        Node left_child = tree_get_node(vm->tree, left_child_index);
        switch (node_get_tag(left_child)) {
            case NODE_TAG_INDIR: {
                debug("Invoking indirection rule (left side)\n");
                // A Ia b -> Aab
                tree_move_child(&vm->tree, left_child_index, CHILD_SIDE_LEFT,
                    top_index, CHILD_SIDE_LEFT);
                break;
            }
            case NODE_TAG_STEM: {
                debug("Invoking rule 0b\n");
                // A Sa b -> Fab
                // Left child is a stem - make a fork
                tree_change_tag(&vm->tree, top_index, NODE_TAG_FORK);
                tree_move_child(&vm->tree, left_child_index, CHILD_SIDE_LEFT,
                    top_index, CHILD_SIDE_LEFT);
                spine_array_pop(&vm->spine, &empty);
                break;
            }
            case NODE_TAG_FORK: {
                _apply_rules(&vm->tree, top_index, top_node, left_child_index,
                    left_child, right_child_index, right_child);
                break;
            }
            case NODE_TAG_APP: {
                spine_array_push(&vm->spine, left_child_index);
                return VM_STATE_RUNNING;
                break;
            }
            default: {
                fail("PANIC! Invalid tag during evaluation: %d\n",
                    node_get_tag(left_child));
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
        debug("--- STEP %lu ---\n", counter++);
        // tree_debug_print(vm->tree);
        // _spine_print(vm->spine);
        state = vm_step(vm);
    }
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
void vm_serialize(vm_write_fn write_fn, struct Vm vm, size_t chunk_size,
    void* ctx)
{
    size_t total_size = vm_get_size(vm);
    size_t written_total = 0;
    struct VmHeader header = vm_default_header;
    header.size = total_size;
    write_fn(ctx, &header, sizeof(struct VmHeader));
    while (total_size > written_total) {
        written_total += write_fn(ctx, vm.tree.nodes.data + written_total,
            min(total_size - written_total, chunk_size));
    }
}

enum VmResult vm_deserialize(struct Vm* vm, vm_read_fn read_fn,
    size_t chunk_size, void* ctx)
{
    struct VmHeader header;
    size_t read = read_fn(ctx, &header, sizeof(struct VmHeader));
    if (read != sizeof(struct VmHeader)) {
        return VM_ERR_TRUNCATED;
    }
    if (strncmp(header.magic, vm_default_header.magic, 4) != 0) {
        return VM_ERR_MAGIC_MISMATCH;
    }
    if (header.version.major > vm_default_header.version.major) {
        return VM_ERR_UNSUPPORTED_VERSION;
    }
    struct VmConfig config = vm_default_config;
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
    if (node_get_child_index(left_child, CHILD_SIDE_LEFT) == 0) {
        // Rule 1
        _apply_rule_1(tree, top_index, top_node, left_child_index, left_child,
            right_child_index);
    } else {
        Index left_of_left_index = node_get_child_index(left_child,
            CHILD_SIDE_LEFT);
        Node left_of_left_child = tree_get_node(*tree, left_of_left_index);
        switch (node_get_tag(left_of_left_child)) {
            case NODE_TAG_STEM: {
                // Rule 2
                _apply_rule_2(tree, top_index, top_node, left_child_index,
                    left_child, right_child_index);
                break;
            }
            case NODE_TAG_FORK: {
                // Rule 3a-3c
                if (right_child_index == 0) {
                    // Rule 3a
                    _apply_rule_3a(tree, top_index, top_node, left_child_index,
                        left_child);
                } else {
                    switch (node_get_tag(right_child)) {
                        case NODE_TAG_STEM: {
                            // Rule 3b
                            _apply_rule_3b(tree, top_index, top_node,
                                left_child_index, left_child, right_child_index,
                                right_child);
                            break;
                        }
                        case NODE_TAG_FORK: {
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
    tree_detach_child(tree, top_index, CHILD_SIDE_RIGHT);
    tree_move_child(tree, left_child_index, CHILD_SIDE_RIGHT, top_index,
        CHILD_SIDE_LEFT);
    tree_change_tag(tree, top_index, NODE_TAG_INDIR);
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
    Index node_S_index = node_get_child_index(left_child, CHILD_SIDE_LEFT);
    Index left_app_index = tree_add_node(tree, NODE_TAG_APP);
    Index right_app_index = tree_add_node(tree, NODE_TAG_APP);
    tree_move_child(tree, node_S_index, CHILD_SIDE_LEFT, left_app_index,
        CHILD_SIDE_LEFT);
    tree_move_child(tree, left_child_index, CHILD_SIDE_RIGHT, right_app_index,
        CHILD_SIDE_LEFT);
    tree_copy_child(tree, top_index, CHILD_SIDE_RIGHT, left_app_index,
        CHILD_SIDE_RIGHT);
    tree_move_child(tree, top_index, CHILD_SIDE_RIGHT, right_app_index,
        CHILD_SIDE_RIGHT);
    tree_change_child(tree, top_index, CHILD_SIDE_LEFT, left_app_index);
    tree_change_child(tree, top_index, CHILD_SIDE_RIGHT, right_app_index);
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
    Index node_lower_F_index = node_get_child_index(left_child,
        CHILD_SIDE_LEFT);
    tree_detach_child(tree, top_index, CHILD_SIDE_RIGHT);
    tree_move_child(tree, node_lower_F_index, CHILD_SIDE_LEFT, top_index,
        CHILD_SIDE_LEFT);
    tree_change_tag(tree, top_index, NODE_TAG_INDIR);
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
    Index node_lower_F_index = node_get_child_index(left_child,
        CHILD_SIDE_LEFT);
    tree_move_child(tree, node_lower_F_index, CHILD_SIDE_RIGHT, top_index,
        CHILD_SIDE_LEFT);
    tree_move_child(tree, right_child_index, CHILD_SIDE_LEFT, top_index,
        CHILD_SIDE_RIGHT);
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
    Index left_app_index = tree_add_node(tree, NODE_TAG_APP);
    tree_move_child(tree, left_child_index, CHILD_SIDE_RIGHT, left_app_index,
        CHILD_SIDE_LEFT);
    tree_move_child(tree, right_child_index, CHILD_SIDE_LEFT, left_app_index,
        CHILD_SIDE_RIGHT);
    tree_change_child(tree, top_index, CHILD_SIDE_LEFT, left_app_index);
    tree_move_child(tree, right_child_index, CHILD_SIDE_RIGHT, top_index,
        CHILD_SIDE_RIGHT);
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

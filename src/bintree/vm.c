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

#define TAG 1

typedef struct {
    uintptr_t addr;
} TaggedNodeMemberAddr;

struct VM {
    struct Tree* tree;
    struct Stack* spine; // Each entry is a TaggedNodeMemberAddr
};

enum DescentDir {LeftDescent = 0, RightDescent = 1};

static TaggedNodeMemberAddr _tag_node_addr
    (struct Node** node_addr, enum DescentDir dir);
static struct Node**        _untag_node_addr    (TaggedNodeMemberAddr tna,
    enum DescentDir* dir);

static struct Stack*        _spine_stack_make           ();
static void                 _spine_stack_push           (struct Stack* stack,
    TaggedNodeMemberAddr root);
static void                 _spine_stack_push_with_tag  (struct Stack* stack,
    struct Node** root, enum DescentDir tag);
static TaggedNodeMemberAddr _spine_stack_pop            (struct Stack* stack);
static void                 _spine_stack_unpop          (struct Stack* stack);
static struct Node**        _spine_stack_pop_with_tag   (struct Stack* stack,
    enum DescentDir* tag);
static struct Node**        _spine_stack_peek_with_tag  (struct Stack* stack,
    enum DescentDir* tag);

static void                 _spine_stack_print  (struct Stack* stack);

static void _apply_rules(struct VM* vm);

static void _apply_rule_1(struct Tree* tree, struct Node** apps[3]);
static void _apply_rule_2(struct Tree* tree, struct Node** apps[3]);
static void _apply_rule_3a(struct Tree* tree, struct Node** apps[3]);
static void _apply_rule_3b(struct Tree* tree, struct Node** apps[3]);
static void _apply_rule_3c(struct Tree* tree, struct Node** apps[3]);

// -----------------------------------------------------------------------------

struct VM* vm_make(struct Tree* tree) {
    struct VM* vm = (struct VM*)malloc(sizeof(struct VM));
    vm->tree = tree;
    vm->spine = _spine_stack_make();
    _spine_stack_push(vm->spine, _tag_node_addr(&tree->root, LeftDescent));
    return vm;
}

enum StepState vm_step(struct VM* vm) {
    // struct Node** - if it is tagged, it means a right-descent was done. The
    // untagged version always contains the left child's address
    // struct Node* - if its right member is tagged, it means this App node is
    // already reduced

    // Peek at the top of the stack
    enum DescentDir dir;
    struct Node** top_addr = _spine_stack_peek_with_tag(vm->spine, &dir);

    // If the address is NULL, the stack was empty
    if (top_addr == NULL) {
        return Done;
    }

    // If the top of the stack points to a leaf, check if the top is the left or
    // right child. If left, go backwards, tag the parent, and push its right
    // child. If right, just go backwards.
    struct Node* top = deref_node_addr(top_addr);
    if (top == NULL) {
        debug("top == NULL\n");
        (void)_spine_stack_pop(vm->spine);
        if (dir == RightDescent) {
            // It was a right-descent
            return Running;
        }

        // It was a left-descent
        // Check if a parent exists, or the root has been reached
        struct Node** parent_addr = _spine_stack_peek_with_tag(vm->spine, &dir);
        if (parent_addr == NULL) {
            return Done;
        }
        // Read the correct address for the parent, and tag the node as reduced
        struct Node* parent = deref_node_addr(parent_addr);
        set_tag_right(parent, (uint8_t)Reduced);
        // Push back top_addr, but tagged
        _spine_stack_push_with_tag(vm->spine, top_addr, RightDescent);
        return Running;
    }

    // If the address points to a zero-ref indir node, delete it
    // NOTE: The reason indirection nodes are deleted here, and not in
    // `delete_node`, is the fact that if the stack spine contains a child of an
    // indirection node, which is on the freelist, and it gets deleted, the
    // node's right member (and therefore the address of its children) becomes
    // invalid, thus invalidating the address found in the spine stack.
    if (get_tag_left(top) == Indir) {
        if (is_zero_ref(top) == TRUE) {
            struct Node* node_to_delete = unset_indir(top);
            freelist_stack_push(vm->tree->freelist, node_to_delete);
        }
    }

    enum NodeTagRight evaled_tag = get_tag_right(top);

    // Either induce reduction, or travel down the left child
    if (evaled_tag == Reduced) {
        debug("evaled_tag == Reduced\n");
        _apply_rules(vm);
    } else {
        debug("evaled_tag == Unreduced\n");
        // FIXME Mark the left child as unreduced, otherwise the reduction gets
        // stuck if this subtree is shared and was already reduced at another
        // point
        struct Node* left_child = deref_node_addr(get_left_addr(top));
        if (left_child != NULL) {
            set_tag_right(left_child, Unreduced);
        }
        _spine_stack_push_with_tag(vm->spine, get_left_addr(top), LeftDescent);
    }

    return Running;
}

void vm_run(struct VM* vm) {
    enum StepState state = Running;

    size_t counter = 0;
    while (state == Running) {
        debug("--- STEP %d ---\n", counter++);
        state = vm_step(vm);
        
        _spine_stack_print(vm->spine);
        draw_tree(".output/output_tree", vm->tree);
        // getchar();
        // if (counter == 50) {
            //     return;
        // }
    }
}

// Serialize the VM's state into a byte array
uint8_t* vm_serialize(struct VM* vm, size_t* size) {
    // Serialize the node stack directly, replacing the pointer addresses with
    // tagged indices. Maintain a translation table (possibly reuse the spine
    // stack for that purpose?) and rewrite addresses based on that table. 0 is
    // NULL, as it is invalid for a subtree to point to the root.
    // Then serialize the spine stack as a list of indices, along with the tags.
    // NOTE Maybe the spine stack doesn't need to be serialized (only the tags),
    // if the nodes are stored in the order they appear in the spine stack?

    // NOTE Serializing a tree can be done in three ways:
    // - As a string of binary digits (1 - APP, 0 - LEAF)
    //  - con: sharing cannot be represented
    // - As an array of Nodes (with two indices per node)
    //  - the size of the pointers would need to be declared
    // - As a list of instructions
}

struct VM* vm_deserialize(uint8_t* data) {
    //
}

// -------------------------------- INTERNAL METHODS ---------------------------
static TaggedNodeMemberAddr _tag_node_addr(struct Node** node_addr,
    enum DescentDir dir)
{
    TaggedNodeMemberAddr tna;
    tna.addr = tag_value((uintptr_t)node_addr, (uint8_t)dir);
    return tna;
}

static struct Node** _untag_node_addr(TaggedNodeMemberAddr tna,
    enum DescentDir* dir)
{
    *dir = (enum DescentDir)tag_of_value(tna.addr);
    struct Node** node_addr = (struct Node**)untag_value(tna.addr);
    if (*dir == RightDescent) {
        // node_addr is always equal to get_left_addr((struct Node*)node_addr),
        // since `left` is the first struct member
        node_addr = get_right_addr((struct Node*)node_addr);
    }
    return node_addr;
}

static struct Stack* _spine_stack_make() {
    return stack_make(SPINE_SEGMENT_SIZE);
}

static void _spine_stack_push(struct Stack* stack, TaggedNodeMemberAddr tna) {
    #ifdef DEBUG_PRINTS
    enum DescentDir dir;
    struct Node** node_addr = _untag_node_addr(tna, &dir);
    if (node_addr == NULL) {
        debug("PUSH: %" PRIuPTR "\n", tna.addr);
    } else {
        debug("PUSH: %" PRIuPTR " (%" PRIuPTR ")\n", tna.addr,
            (uintptr_t)*node_addr);
    }
    #endif

    TaggedNodeMemberAddr* slot = (TaggedNodeMemberAddr*)stack_alloc(stack,
        SPINE_SEGMENT_SIZE, sizeof(TaggedNodeMemberAddr));
    *slot = tna;
}

static void _spine_stack_push_with_tag(struct Stack* stack, struct Node** root,
    enum DescentDir tag)
{
    _spine_stack_push(stack, _tag_node_addr(root, tag));
}

static TaggedNodeMemberAddr _spine_stack_pop(struct Stack* stack) {
    TaggedNodeMemberAddr* result =
        (TaggedNodeMemberAddr*)stack_pop(stack, sizeof(TaggedNodeMemberAddr));
    if (result == NULL) {
        debug("POP NULL\n");
        TaggedNodeMemberAddr tna = {(uintptr_t)NULL};
        return tna;
    } else {
        debug("POP %" PRIuPTR "\n", result->addr);
    }
    return *result;
}

static void _spine_stack_unpop(struct Stack* stack) {
    debug("UNPOP\n");
    stack_unpop(stack, sizeof(struct Node**));
}

static struct Node** _spine_stack_pop_with_tag(struct Stack* stack,
    enum DescentDir* tag)
{
    TaggedNodeMemberAddr value = _spine_stack_pop(stack);
    return _untag_node_addr(value, tag);
}

static struct Node** _spine_stack_peek_with_tag(struct Stack* stack,
    enum DescentDir* tag)
{
    TaggedNodeMemberAddr* result =
        (TaggedNodeMemberAddr*)stack_peek(stack, sizeof(TaggedNodeMemberAddr));
    if (result == NULL) {
        return NULL;
    }
    return _untag_node_addr(*result, tag);
}

static void _spine_stack_print(struct Stack* stack) {
    #ifdef DEBUG_PRINTS
    uint8_t* data = segment_get_data(stack->current_segment);
    uint8_t* end = segment_get_next_free_addr(stack->current_segment);

    size_t i = 0;
    while (data != end) {
        TaggedNodeMemberAddr* node = (TaggedNodeMemberAddr*)data;
        printf("%lu: %" PRIuPTR "\n", i, node->addr);
        data += sizeof(TaggedNodeMemberAddr);
        i++;
    }
    #endif
}

// NOTE: It is tempting to reuse application nodes in the reduction rules,
// especially in rule #2 (t(ta)bc -> ac(bc)), but that is a mistake: if any of
// the subtrees are shared, the application nodes cannot be reorganized without
// corrupting the subtree. Therefore, the whole subtree is marked for deletion
// and the required parts are duplicated instead.
static void _apply_rules(struct VM* vm) {
    struct Node** apps[3]; // apps[2] is the root
    enum DescentDir dir;
    TaggedNodeMemberAddr apps_temp;
    
    // Check if this is a 3rd child
    bool_t third_child = FALSE;
    apps_temp = _spine_stack_pop(vm->spine);
    apps[0] = _untag_node_addr(apps_temp, &dir);
    // apps[0] = _spine_stack_pop_with_tag(vm->spine, &dir);
    if (apps[0] == NULL) {
        return;
    }
    apps[1] = get_left_addr(deref_node_addr(apps[0]));
    struct Node* temp = deref_node_addr(apps[1]);
    if (temp != NULL) {
        apps[2] = get_left_addr(temp);
        temp = deref_node_addr(apps[2]);
        if (temp != NULL) {
            if (get_tag_right(temp) == Reduced) {
                third_child = TRUE;
                debug("Third child!\n");
            }
        }
    }

    if (third_child == FALSE) {
        if (dir == RightDescent) {
            return;
        }
        // Check if there are enough children to induce reduction
        apps_temp = _spine_stack_pop(vm->spine);
        apps[1] = _untag_node_addr(apps_temp, &dir);
        if (apps[1] == NULL) {
            return;
        }
        if (dir == RightDescent || (stack_is_empty(vm->spine) == TRUE)) {
            // This is a second child, and there are only two children. If it is
            // unreduced, invoke its reduction
            struct Node* apps_1_deref = deref_node_addr(apps[1]);
            if (get_tag_right(apps_1_deref) == Unreduced) {
                set_tag_right(apps_1_deref, Reduced);
                _spine_stack_push(vm->spine, apps_temp);
                _spine_stack_push_with_tag(vm->spine,
                    get_left_addr(apps_1_deref), RightDescent);
            }
            return;
        }

        apps_temp = _spine_stack_pop(vm->spine);
        apps[2] = _untag_node_addr(apps_temp, &dir);
        if (apps[2] == NULL) {
            return;
        }
    }

    // Count the number of children of the current node
    uint8_t child_count = 0;
    struct Node* child0 = get_right(deref_node_addr(apps[0]));
    debug("Child0: %" PRIuPTR "\n", (uintptr_t)child0);
    while (!is_leaf(child0) && child_count < 3) {
        child_count++;
        child0 = get_left(child0);
    }
    debug("%d children\n", child_count);

    if (third_child == TRUE) {
        // Current top is the third child
        // Swap the apps order
        struct Node** temp_apps = apps[0];
        apps[0] = apps[2];
        apps[2] = temp_apps;

        struct Node* node_to_delete = deref_node_addr(apps[2]);
        // Invoke rule 3a, 3b or 3c
        if (child_count == 0) {
            _apply_rule_3a(vm->tree, apps);
        } else {
            if (child_count == 1) {
                _apply_rule_3b(vm->tree, apps);
            } else {
                _apply_rule_3c(vm->tree, apps);
            }
        }
        delete_node(vm->tree, node_to_delete);
    } else {
        struct Node* node_to_delete = deref_node_addr(apps[2]);
        // Current top is the first child
        if (child_count > 1) {
            // Push the top back
            // _spine_stack_push_with_tag(vm->spine, apps[2], dir);
            _spine_stack_push(vm->spine, apps_temp);
            set_tag_right(deref_node_addr(apps[2]), (uint8_t)Reduced);

            // And then induce reduction of the top's right child as well
            _spine_stack_push_with_tag(vm->spine,
                get_left_addr(deref_node_addr(apps[2])), RightDescent);
            return;
        } else {
            // Invoke rule 1 or 2
            if (child_count == 0) {
                _apply_rule_1(vm->tree, apps);
            } else {
                _apply_rule_2(vm->tree, apps);
            }
            delete_node(vm->tree, node_to_delete);
        }
    }

    // Push the root to the stack
    _spine_stack_push(vm->spine, apps_temp);
    // draw_tree(".output/output_tree", vm->tree);
}

static void _apply_rule_1(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 1\n");
    // printf("Invoking rule 1\n");
    duplicate_node_to(tree, get_right(deref_node_addr(apps[1])), apps[2]);
}

static void _apply_rule_2(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 2\n");
    // printf("Invoking rule 2\n");
    struct Node* child0 = get_right(deref_node_addr(apps[0]));
    struct Node* child1 = get_right(deref_node_addr(apps[1]));
    struct Node* child2 = get_right(deref_node_addr(apps[2]));

    struct Node* left = add_node(tree, NULL, NULL);
    struct Node* right = add_node(tree, NULL, NULL);
    struct Node* top = add_node(tree, left, right);
    duplicate_node_to(tree, get_right(child0), get_left_addr(left));
    duplicate_node_to(tree, child2, get_right_addr(left));
    duplicate_node_to(tree, child1, get_left_addr(right));
    duplicate_node_to(tree, child2, get_right_addr(right));
    // *((struct Node**)untag_value((uintptr_t)apps[2])) = top;
    set_value_at_node_addr(apps[2], top);
}

static void _apply_rule_3a(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 3a\n");
    // printf("Invoking rule 3a\n");
    struct Node* child0 = get_right(deref_node_addr(apps[0]));
    duplicate_node_to(tree, get_right(get_left(child0)), apps[2]);
}

static void _apply_rule_3b(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 3b\n");
    // printf("Invoking rule 3b\n");
    struct Node* child0 = get_right(deref_node_addr(apps[0]));
    struct Node* child2 = get_right(deref_node_addr(apps[2]));

    struct Node* top = add_node(tree, NULL, NULL);
    duplicate_node_to(tree, get_right(child0), get_left_addr(top));
    duplicate_node_to(tree, get_right(child2), get_right_addr(top));
    // *((struct Node**)untag_value((uintptr_t)apps[2])) = top;
    set_value_at_node_addr(apps[2], top);
}

static void _apply_rule_3c(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 3c\n");
    // printf("Invoking rule 3c\n");
    struct Node* child0 = get_right(deref_node_addr(apps[0]));
    struct Node* child1 = get_right(deref_node_addr(apps[1]));
    struct Node* child2 = get_right(deref_node_addr(apps[2]));

    struct Node* left = add_node(tree, NULL, NULL);
    struct Node* top = add_node(tree, left, NULL);
    duplicate_node_to(tree, child1, get_left_addr(left));
    duplicate_node_to(tree, get_right(get_left(child2)),
        get_right_addr(left));
    duplicate_node_to(tree, get_right(child2), get_right_addr(top));
    // *((struct Node**)untag_value((uintptr_t)apps[2])) = top;
    set_value_at_node_addr(apps[2], top);
}
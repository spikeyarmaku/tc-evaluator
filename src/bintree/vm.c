// Idea:
// What if init_program consists of just binary code for instructions on how to
// build the tree, and the runtime reduces the tree as it is building it?
// so e.g. it gets the instructions AAALLLL, and by the second L, it realizes
// that rule 1 has to be invoked

// Idea:
// What if children are stored in a reverse order? This way the whole tree would
// not needed to be traversed to find the first redex, because that would be on
// the top.

// TODO Can there ever be a NULL node that is behind an indirection node?

// TODO Currently the VM traverses shared subtrees multiple times, because it
// does not remember already traversed nodes. This is a potential optimization
// path. (For an example, take the tree tt(t(t(t(ttt)))(tt)(t(ttt))tt)t which
// will induce rule 2, rule 3c, and then 3b. Between these two runs, the subtree
// shared in rule 2 gets traversed twice.) One idea: use the refcount part of
// the node to store the fact that it has already been traversed? Downside: it
// would certainly violate some code structuring guidelines, unless the code
// responsible for tagging is restructured.

// TODO Why does this runtime eat 6 gigs of RAM on fib 28?

#include "vm.h"

#include <inttypes.h>

#define TAG 1

struct VM {
    struct Tree* tree;

    struct Stack* spine;
};

enum EvalTag {Unevaled, Evaled};

static enum EvalTag     _get_eval_tag   (struct Node** node);
static struct Node**    _set_eval_tag   (struct Node** node, enum EvalTag tag);

static struct Stack*    _spine_stack_make   ();
static void             _spine_stack_push   (struct Stack* stack,
    struct Node** root);
static void             _spine_stack_push_with_tag  (struct Stack* stack,
        struct Node** root, enum EvalTag tag);
static struct Node**    _spine_stack_pop    (struct Stack* stack);
static void             _spine_stack_unpop  (struct Stack* stack);
static struct Node**    _spine_stack_peek   (struct Stack* stack);

static struct Node**    _spine_stack_pop_with_tag   (struct Stack* stack,
    enum EvalTag* tag);
static struct Node**    _spine_stack_peek_with_tag  (struct Stack* stack,
    enum EvalTag* tag);

static void             _spine_stack_print  (struct Stack* stack);

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
    _spine_stack_push(vm->spine, &tree->root);
    return vm;
}

bool_t vm_step(struct VM* vm) {
    debug("--- NEW STEP ---\n");
    
    // Peek at the top of the stack
    enum EvalTag top_addr_tag;
    struct Node** top_addr =
        _spine_stack_peek_with_tag(vm->spine, &top_addr_tag);

    // If the address is NULL, either the stack was empty, or a right traversal
    // just ended
    if (top_addr == NULL) {
        debug("top_addr == NULL\n");
        // Pop another, and see if it is still NULL
        if (_spine_stack_pop(vm->spine) == NULL) {
            debug("--- EMPTY STACK, reduction is over ---\n");
            return TRUE;
        }
        _spine_stack_unpop(vm->spine);
        return FALSE;
    }

    // If the top of the stack points to a leaf, check if the top is the left or
    // right child (NULL above in stack). If left, go backwards, tag the parent,
    // and push its right child. If right, just go backwards and pop the NULL.
    struct Node* top = *top_addr;
    if (top == NULL) {
        debug("top == NULL\n");
        (void)_spine_stack_pop(vm->spine);
        struct Node** temp = _spine_stack_pop(vm->spine);
        if (temp == NULL) {
            return FALSE;
        }

        _spine_stack_push(vm->spine,
            (struct Node**)tag_value((uintptr_t)temp, Evaled));
        // Push a NULL to mark a right-descend
        _spine_stack_push(vm->spine, NULL);
        _spine_stack_push(vm->spine, get_right_addr(*temp));
        return FALSE;
    }

    // Either induce reduction, or travel down the left child
    if (top_addr_tag == Evaled) {
        debug("top_addr_tag == Evaled\n");
        _apply_rules(vm);
    } else {
        debug("top_addr_tag == Unevaled\n");
        _spine_stack_push(vm->spine, get_left_addr(top));
    }

    return FALSE;
}

void vm_run(struct VM* vm) {
    bool_t done = FALSE;

    while (done == FALSE) {
        done = vm_step(vm);

        _spine_stack_print(vm->spine);
        // getchar();
    }
}

// -------------------------------- INTERNAL METHODS ---------------------------
static enum EvalTag _get_eval_tag(struct Node** node) {
    return (enum EvalTag)tag_of_value((uintptr_t)node);
}

static struct Node** _set_eval_tag(struct Node** node, enum EvalTag tag) {
    return (struct Node**)tag_value((uintptr_t)node, (uint8_t)tag);
}

static struct Stack* _spine_stack_make() {
    return stack_make(SPINE_SEGMENT_SIZE);
}

static void _spine_stack_push(struct Stack* stack, struct Node** node) {
    if ((struct Node**)untag_value((uintptr_t)node) == NULL) {
        debug("PUSH: %" PRIuPTR "\n", (uintptr_t)node);
    } else {
        debug("PUSH: %" PRIuPTR " (%" PRIuPTR ")\n", (uintptr_t)node,
            (uintptr_t)*(struct Node**)untag_value((uintptr_t)node));
    }
    struct Node*** slot = (struct Node***)stack_alloc(stack, SPINE_SEGMENT_SIZE,
        sizeof(struct Node*));
    *slot = node;
}

static void _spine_stack_push_with_tag(struct Stack* stack, struct Node** root,
    enum EvalTag tag)
{
    _spine_stack_push(stack, (struct Node**)tag_value((uintptr_t)root,
        (uint8_t)tag));
}

static struct Node** _spine_stack_pop(struct Stack* stack) {
    struct Node*** result =
        (struct Node***)stack_pop(stack, sizeof(struct Node**));
    if (result == NULL) {
        debug("POP NULL\n");
        return NULL;
    } else {
        debug("POP %" PRIuPTR "\n", (uintptr_t)*result);
    }
    return *result;
}

static void _spine_stack_unpop(struct Stack* stack) {
    stack_unpop(stack, sizeof(struct Node**));
}

static struct Node** _spine_stack_pop_with_tag(struct Stack* stack,
    enum EvalTag* tag)
{
    struct Node** value = _spine_stack_pop(stack);
    *tag = tag_of_value((uintptr_t)value);
    return (struct Node**)untag_value((uintptr_t)value);
}

static struct Node** _spine_stack_peek(struct Stack* stack) {
    struct Node*** result =
        (struct Node***)stack_peek(stack, sizeof(struct Node**));
    if (result == NULL) {
        return NULL;
    }
    return *result;
}

static struct Node** _spine_stack_peek_with_tag(struct Stack* stack,
    enum EvalTag* tag)
{
    struct Node** value = (struct Node**)_spine_stack_peek(stack);
    *tag = tag_of_value((uintptr_t)value);
    return (struct Node**)untag_value((uintptr_t)value);
}

static void _spine_stack_print(struct Stack* stack) {
    #ifdef DEBUG_PRINTS
    uint8_t* data = segment_get_data(stack->current_segment);
    uint8_t* end = segment_get_next_free_addr(stack->current_segment);

    size_t i = 0;
    while (data != end) {
        struct Node** node = (struct Node**)data;
        printf("%lu: %" PRIuPTR "\n", i, (uintptr_t)*node);
        data += sizeof(struct Node**);
        i++;
    }
    #endif
}

// NOTE: It is tempting to reuse application nodes in the reduction rules,
// especially in rule #2 (t(ta)bc -> ac(bc)), but that is a mistake: if any of
// the subtrees are shared, the application nodes cannot be reorganized without
// corrupting the subtree. Therefore, the whole subtree is marked for deletion
// and the required parts are duplicated instead.

// If the current top is evaled, the following cases are possible:
// - There are not enough children to induce reduction (second pop returns NULL)
// - There are, and the second pop returns a tagged node address (rules 3a-3c)
// - There are, and the current node is the first child
static void _apply_rules(struct VM* vm) {
    // Check if there are enough children to induce reduction
    enum EvalTag tags[3];
    struct Node** apps[3];
    for (uint8_t i = 0; i < 3; i++) {
        // apps[2] is the root
        apps[i] = _spine_stack_pop_with_tag(vm->spine, &tags[i]);
    }
    assert(apps[0] != NULL);

    if (apps[2] == NULL) {
        if (apps[1] != NULL) {
            // There aren't enough children to induce reduction
            _spine_stack_push(vm->spine, NULL);
            _spine_stack_push_with_tag(vm->spine, apps[1], Evaled);
            _spine_stack_push(vm->spine, NULL);
            _spine_stack_push(vm->spine, get_right_addr(*apps[1]));
        }
        return;
    }

    if (apps[1] == NULL) {
        // There aren't enough children to induce reduction
        _spine_stack_push_with_tag(vm->spine, apps[2], tags[2]);
        return;
    }

    // Count the number of children of the current node
    uint8_t child_count = 0;
    struct Node* child0 = get_right(*apps[0]);
    while (!is_leaf(child0) && child_count < 3) {
        child_count++;
        child0 = get_left(child0);
    }
    debug("%d children\n", child_count);

    if (tags[2] == (uint8_t)Evaled) {
        // Current top is the third child
        // Reorder apps
        struct Node** temp = apps[2];
        apps[2] = apps[0];
        apps[0] = temp;

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
    } else {
        // Current top is the first child
        if (child_count > 1) {
            // Push the terms back in the same order that they were popped,
            // which means they will be pushed onto the stack in the reverse
            // order, since a stack is a LIFO structure
            _spine_stack_push_with_tag(vm->spine, apps[0], tags[0]);
            _spine_stack_push_with_tag(vm->spine, apps[1], tags[1]);
            _spine_stack_push_with_tag(vm->spine, apps[2], Evaled);

            // And then induce reduction of the top child as well
            _spine_stack_push(vm->spine, NULL);
            _spine_stack_push(vm->spine, get_right_addr(*apps[2]));
            return;
        } else {
            // Invoke rule 1 or 2
            if (child_count == 0) {
                _apply_rule_1(vm->tree, apps);
            } else {
                _apply_rule_2(vm->tree, apps);
            }
        }
    }

    _spine_stack_push(vm->spine, apps[2]);
    draw_tree(".output/output_tree", vm->tree);
}

static void _apply_rule_1(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 1\n");
    duplicate_node_to(tree, get_right(*apps[1]), apps[2]);
}

static void _apply_rule_2(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 2\n");
    struct Node* child0 = get_right(*apps[0]);
    struct Node* child1 = get_right(*apps[1]);
    struct Node* child2 = get_right(*apps[2]);

    struct Node* left = add_node(tree, NULL, NULL);
    struct Node* right = add_node(tree, NULL, NULL);
    struct Node* top = add_node(tree, left, right);
    duplicate_node_to(tree, get_right(child0), get_left_addr(left));
    duplicate_node_to(tree, child2, get_right_addr(left));
    duplicate_node_to(tree, child1, get_left_addr(right));
    duplicate_node_to(tree, child2, get_right_addr(right));
    *apps[2] = top;
}

static void _apply_rule_3a(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 3a\n");
    struct Node* child0 = get_right(*apps[0]);
    duplicate_node_to(tree, get_right(get_left(child0)), apps[2]);
}

static void _apply_rule_3b(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 3b\n");
    struct Node* child0 = get_right(*apps[0]);
    struct Node* child2 = get_right(*apps[2]);

    struct Node* top = add_node(tree, NULL, NULL);
    duplicate_node_to(tree, get_right(child0), get_left_addr(top));
    duplicate_node_to(tree, get_right(child2), get_right_addr(top));
    *apps[2] = top;
}

static void _apply_rule_3c(struct Tree* tree, struct Node** apps[3]) {
    debug("Invoking rule 3c\n");
    struct Node* child0 = get_right(*apps[0]);
    struct Node* child1 = get_right(*apps[1]);
    struct Node* child2 = get_right(*apps[2]);

    struct Node* left = add_node(tree, NULL, NULL);
    struct Node* top = add_node(tree, left, NULL);
    duplicate_node_to(tree, child1, get_left_addr(left));
    duplicate_node_to(tree, get_right(get_left(child2)),
        get_right_addr(left));
    duplicate_node_to(tree, get_right(child2), get_right_addr(top));
    *apps[2] = top;
}
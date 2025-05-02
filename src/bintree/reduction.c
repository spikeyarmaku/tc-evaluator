#include "reduction.h"

struct Reduct make_reduct() {
    struct Reduct reduct;
    reduct.up_depth = 0;
    reduct.root = NULL;
    for (int i = 0; i < 3; i++) {
        reduct.apps[i] = NULL;
    }
    return reduct;
}

void print_reduct(int ind, struct Reduct reduct) {
    if (reduct.root == NULL) {
        debug_indent(ind, "Reduct:\n");
        debug_indent(ind, "up_depth: %d\n", reduct.up_depth);
        debug_indent(ind, "root: (NO ROOT)\n");
        debug_indent(ind, "children:\n");
    } else {
        debug_indent(ind, "Reduct:\n");
        debug_indent(ind, "up_depth: %d\n", reduct.up_depth);
        debug_indent(ind, "root: %lu\n", (size_t)reduct.root);
        debug_indent(ind, "*root: %lu\n", (size_t)*reduct.root);
        debug_indent(ind, "children:\n");
    }
    for (int i = 0; i < 3; i++) {
        debug_indent(ind, "%d: %lu\n", i, (size_t)reduct.apps[i]);
    }
}

// Apply reduction rules to a subtree
void apply_rules(int ind, struct Tree* tree, struct Reduct reduct) {
    debug_indent(ind, "apply_rules root: %lu\n", (size_t)*reduct.root);
    print_reduct(ind, reduct);

    struct Node* child0 = get_right(reduct.apps[0]);

    // NOTE: It is tempting to reuse application nodes in the reduction rules,
    // especially in rule #2 (t(ta)bc -> ac(bc)), but that is a mistake: if any
    // of the subtrees are shared, the application nodes cannot be reorganized
    // without corrupting the subtree. Therefore, the whole subtree is marked
    // for deletion and the required parts are duplicated instead.

    if (is_leaf(child0) == TRUE) {
        // ttab -> a     (first child is a leaf)
        debug_indent(ind, "Invoking rule #1: ttab -> a\n");
        // duplicate_node_to(tree, get_right_addr(reduct.apps[1]), reduct.root);
        duplicate_node_to(tree, get_right(reduct.apps[1]), reduct.root);
        return;
    }

    if (is_leaf(get_left(child0)) == TRUE) {
        // t(ta)bc -> ac(bc)     (first child is a stem)
        debug_indent(ind, "Invoking rule #2: t(ta)bc -> ac(bc)\n");
        struct Node* left = add_node(tree, NULL, NULL);
        struct Node* right = add_node(tree, NULL, NULL);
        struct Node* top = add_node(tree, left, right);
        duplicate_node_to(tree, get_right(child0), get_left_addr(left));
        duplicate_node_to(tree, get_right(reduct.apps[2]),
            get_right_addr(left));
        duplicate_node_to(tree, get_right(reduct.apps[1]),
            get_left_addr(right));
        duplicate_node_to(tree, get_right(reduct.apps[2]),
            get_right_addr(right));
        *reduct.root = top;
        return;
    }

    // first child is a fork
    // Reduce last child
    struct Node* child2 = get_right(reduct.apps[2]);
    print_tree(ind, tree);
    if (child2 != NULL) {
        (void)reduce(ind + 1, tree, get_right_addr(reduct.apps[2]), 0);
        child2 = get_right(reduct.apps[2]);
    }

    if (is_leaf(child2) == TRUE) {
        debug_indent(ind, "Invoking rule #3: t(tab)ct -> a\n");
        // t(tab)ct -> a
        duplicate_node_to(tree, get_right(get_left(child0)), reduct.root);
        return;
    }

    if (is_leaf(get_left(child2)) == TRUE) {
        debug_indent(ind, "Invoking rule #4: t(tab)c(tu) -> bu\n");
        // t(tab)c(tu) -> bu
        struct Node* top = add_node(tree, NULL, NULL);
        duplicate_node_to(tree, get_right(child0), get_left_addr(top));
        duplicate_node_to(tree, get_right(child2), get_right_addr(top));
        *reduct.root = top;
        return;
    }

    if (is_leaf(get_left(get_left(child2))) == TRUE) {
        debug_indent(ind, "Invoking rule #5: t(tab)c(tuv) -> cuv\n");
        // t(tab)c(tuv) -> cuv
        struct Node* left = add_node(tree, NULL, NULL);
        struct Node* top = add_node(tree, left, NULL);
        duplicate_node_to(tree, get_right(reduct.apps[1]), get_left_addr(left));
        duplicate_node_to(tree, get_right(get_left(child2)),
            get_right_addr(left));
        duplicate_node_to(tree, get_right(child2), get_right_addr(top));
        *reduct.root = top;
        return;
    }

    // PANIC
    printf("\n=== PANIC! ===\n");
    exit(1);
}

// Traverse and reduce the tree
// NOTE reduce takes the address of the pointer to the current node because it
// might need to replace the pointer itself to a new node (i.e. with rule #2)
// NOTE down_depth is necessary to check if we have to induce reduction of
// children at the lower levels
struct Reduct reduce(int ind, struct Tree* tree, struct Node** node_addr,
    size_t down_depth)
{
    assert(node_addr != NULL);
    debug_indent(ind, "Reducing node: %lu (at %lu)\n",
        (size_t)*node_addr, (size_t)node_addr);

    // Check if the node is null
    if (*node_addr == NULL) {
        struct Reduct reduct = make_reduct();
        reduct.up_depth = 0;
        return reduct;
    }

    // If not, continue traversing the left child
    struct Node** left_child_addr = get_left_addr(*node_addr);
    struct Reduct reduct = reduce(ind + 1, tree, left_child_addr,
        down_depth + 1);

    reduct.apps[reduct.up_depth] = *node_addr;
    // Check if we are at a point where we can reduce the subtree
    if (reduct.up_depth == 2) {
        // If we can reduce, reduce
        reduct.root = node_addr;
        struct Node* node_to_delete = *node_addr;
        apply_rules(ind, tree, reduct);
        delete_node(tree, node_to_delete);
        print_tree(ind, tree);
        draw_tree(".output/output_tree", tree);

        return reduce(ind, tree, node_addr, down_depth);
    } else {
        if (reduct.up_depth == 0 || reduct.up_depth + down_depth < 2) {
            // This is either the first child's parent, or there are not enough
            // children to induce reduction in the lower levels. In either case,
            // reduce the right child
            reduce(ind, tree, get_right_addr(*node_addr), 0);
        }
        
        // Otherwise add the current node to the reduct
        reduct.up_depth++;
        return reduct;
    }
}

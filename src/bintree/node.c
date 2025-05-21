#include "node.h"

// ------------------------------ PUBLIC METHODS ------------------------------

// Get the lowest bit of the left node
// TODO If NodeTag gets more than two possible values, the right child can also
// be tagged for 2 additional values
enum NodeTag get_tag(const struct Node* node) {
    assert(node != NULL);
    return tag_of_value(node->left);
}

// Set the lowest bit of the node to the provided tag
void set_tag(struct Node* node, enum NodeTag tag) {
    node->left = tag_value(node->left, tag);
}

struct Node* get_left(const struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_left((struct Node*)node->right);
    }
    return *get_left_addr(node);
}

struct Node* get_right(const struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_right((struct Node*)node->right);
    }
    return *get_right_addr(node);
}

struct Node** get_left_addr(const struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_left_addr((struct Node*)node->right);
    }
    return (struct Node**)&(node->left);
}

struct Node** get_right_addr(const struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_right_addr((struct Node*)node->right);
    }
    return (struct Node**)&(node->right);
}

struct Node** get_indir_child_addr(const struct Node* node) {
    assert(get_tag(node) == Indir);
    return (struct Node**)&(node->right);
}

struct Node* get_indir_child(const struct Node* node) {
    assert(get_tag(node) == Indir);
    return *get_indir_child_addr(node);
}

void set_left(struct Node* node, const struct Node* left) {
    node->left = (uintptr_t)left;
}

void set_right(struct Node* node, const struct Node* right) {
    node->right = (uintptr_t)right;
}

void set_left_right(struct Node* node, const struct Node* left,
    const struct Node* right)
{
    node->left = (uintptr_t)left;
    node->right = (uintptr_t)right;
}

bool_t is_leaf(const struct Node* node) {
    if (node == NULL) {
        return TRUE;
    }
    if (get_tag(node) == Indir) {
        return is_leaf((struct Node*)node->right);
    }
    return FALSE;
}

// Mark this app node as a ref-counted indirection to another app node
void set_indir_to(struct Node* node, const struct Node* new_node) {
    node->right = (uintptr_t)new_node;
    set_tag(node, Indir);
}

bool_t is_zero_ref(const struct Node* node) {
    assert(get_tag(node) == Indir);
    if ((node->left) == 1) {
        return TRUE;
    }
    return FALSE;
}

void incr_ref(struct Node* node) {
    assert(get_tag(node) == Indir);
    node->left += 4;
}

void decr_ref(struct Node* node) {
    assert(get_tag(node) == Indir);
    node->left -= 4;
}

// Replace the node with its right child's content, and return the original
// right child's address
struct Node* unset_indir(struct Node* node) {
    // Overwrite the original node with the children of its right child
    struct Node* result = (struct Node*)node->right;
    struct Node* left_child = get_left(node);
    struct Node* right_child = get_right(node);
    set_left(node, left_child);
    set_right(node, right_child);

    // Null out the right child
    set_left_right(result, NULL, NULL);
    return result;
}

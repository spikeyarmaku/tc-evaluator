#include "node.h"

size_t _tag_value(size_t value, uint8_t tag);
size_t _untag_value(size_t value);
uint8_t _tag_of_value(size_t value);

// ------------------------------ PUBLIC METHODS ------------------------------

// Get the lowest 2 bits of the node
enum Tag get_tag(struct Node* node) {
    assert(node != NULL);
    return _tag_of_value(node->left);
}

// Set the lowest 2 bits of the node to the provided tag
void set_tag(struct Node* node, enum Tag tag) {
    node->left = _tag_value(node->left, tag);
}

// TODO Handle numbers as well
struct Node* get_left(struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_left((struct Node*)node->right);
    }
    return *get_left_addr(node);
}

struct Node* get_right(struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_right((struct Node*)node->right);
    }
    return *get_right_addr(node);
}

struct Node** get_left_addr(struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_left_addr((struct Node*)node->right);
    }
    return (struct Node**)&(node->left);
}

struct Node** get_right_addr(struct Node* node) {
    if (get_tag(node) == Indir) {
        return get_right_addr((struct Node*)node->right);
    }
    return (struct Node**)&(node->right);
}

struct Node** get_indir_child_addr(struct Node* node) {
    assert(get_tag(node) == Indir);
    return (struct Node**)&(node->right);
}

struct Node* get_indir_child(struct Node* node) {
    assert(get_tag(node) == Indir);
    return *get_indir_child_addr(node);
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

BOOL is_leaf(struct Node* node) {
    if (node == NULL) {
        return TRUE;
    }
    if (get_tag(node) == Indir) {
        return is_leaf((struct Node*)node->right);
    }
    return FALSE;
}

// Mark this app node as a ref-counted indirection to another app node
void set_indir_to(struct Node* node, struct Node* new_node) {
    node->right = (size_t)new_node;
    set_tag(node, Indir);
}

BOOL is_zero_ref(struct Node* node) {
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

// ----------------------------- INTERNAL METHODS -----------------------------

// Return a tagged version of a value, removing previous tags
size_t _tag_value(size_t value, uint8_t tag) {
    return _untag_value(value) + tag;
}

// Return an untagged version of a value
size_t _untag_value(size_t value) {
    return value & ~(size_t)3;
}

// Return the tag of a tagged value
uint8_t _tag_of_value(size_t value) {
    return (uint8_t)(value & 3);
}
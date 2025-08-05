#include "node.h"

#include <inttypes.h>

// ------------------------------ PUBLIC METHODS ------------------------------

// Get the lowest bit of the left node
enum NodeTagLeft get_tag_left(const struct Node* node) {
    assert(node != NULL);
    return tag_of_value(node->left);
}

// Set the lowest bit of the node to the provided tag
void set_tag_left(struct Node* node, enum NodeTagLeft tag) {
    node->left = tag_value(node->left, tag);
}

enum NodeTagRight get_tag_right(const struct Node* node) {
    assert(node != NULL);
    return tag_of_value(node->right);
}

void set_tag_right(struct Node* node, enum NodeTagRight tag) {
    debug("Setting tag %d for %" PRIuPTR "\n", (uint8_t)tag, (uintptr_t)node);
    node->right = tag_value(node->right, tag);
}

struct Node* get_left(const struct Node* node) {
    if (get_tag_left(node) == Indir) {
        return get_left((struct Node*)untag_value(node->right));
    }
    return *get_left_addr(node);
}

struct Node* get_right(const struct Node* node) {
    if (get_tag_left(node) == Indir) {
        return get_right((struct Node*)untag_value(node->right));
    }
    return deref_node_addr(get_right_addr(node));
}

struct Node** get_left_addr(const struct Node* node) {
    if (get_tag_left(node) == Indir) {
        return get_left_addr((struct Node*)untag_value(node->right));
    }
    return (struct Node**)&(node->left);
}

struct Node** get_right_addr(const struct Node* node) {
    if (get_tag_left(node) == Indir) {
        return get_right_addr((struct Node*)untag_value(node->right));
    }
    return (struct Node**)&(node->right);
}

struct Node** get_indir_child_addr(const struct Node* node) {
    assert(get_tag_left(node) == Indir);
    return (struct Node**)&(node->right);
}

struct Node* get_indir_child(const struct Node* node) {
    assert(get_tag_left(node) == Indir);
    return deref_node_addr(get_indir_child_addr(node));
}

struct Node* deref_node_addr(struct Node** node_addr) {
    assert(node_addr != NULL);
    return (struct Node*)untag_value((uintptr_t)*node_addr);
}

void set_value_at_node_addr(struct Node** node_addr, struct Node* value) {
    *(struct Node**)untag_value((uintptr_t)node_addr) = value;
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
    if ((struct Node*)untag_value((uintptr_t)node) == NULL) {
        return TRUE;
    }
    if (get_tag_left(node) == Indir) {
        return is_leaf((struct Node*)untag_value(node->right));
    }
    return FALSE;
}

// Mark this app node as a ref-counted indirection to another app node
// TODO Make it so that if the LSB of the left tag is 1, one of the remaining
// bits marks user-defined values. So ...01 is indir, and ...11 is anything else
// the user defines. It could be used e.g. to store numbers more efficiently.
// There could be hooks registered by the host environment dealing with the
// interpretation of these bits.
void set_indir_to(struct Node* node, const struct Node* new_node) {
    node->right = (uintptr_t)new_node;
    node->left = 0;
    set_tag_left(node, Indir);
}

bool_t is_zero_ref(const struct Node* node) {
    assert(get_tag_left(node) == Indir);
    if ((node->left) == 1) {
        return TRUE;
    }
    return FALSE;
}

void incr_ref(struct Node* node) {
    debug("Increasing refcount from %" PRIuPTR " to %" PRIuPTR "\n",
        node->left, node->left + 2);
    assert(get_tag_left(node) == Indir);
    node->left += 2;
}

void decr_ref(struct Node* node) {
    assert(get_tag_left(node) == Indir);
    node->left -= 2;
}

// Replace the node with its right child's content, and return the original
// right child's address
struct Node* unset_indir(struct Node* node) {
    // Overwrite the original node with the children of its right child
    struct Node* result = (struct Node*)untag_value(node->right);
    uint8_t tag = tag_of_value(node->right);
    struct Node* left_child = get_left(node);
    struct Node* right_child = get_right(node);
    set_left(node, left_child);
    set_right(node, (struct Node*)tag_value((uintptr_t)right_child, tag));

    // Null out the right child
    set_left_right(result, NULL, NULL);
    return result;
}

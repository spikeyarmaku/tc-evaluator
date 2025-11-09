#include "test.h"

#include "../array.h"

void print_node_bits(Node node);

void array_test() {
    struct Array node_arr = array_make(64);
    size_t cap0 = node_arr.capacity;
    Node node0 = node_make(Stem, 1, 1, 0);
    Index ix0 = node_array_push(&node_arr, node0);
    Node node1 = node_make(Stem, 1, 2, 0);
    Index ix1 = node_array_push(&node_arr, node1);
    check("Array indices", ix0 == 0 && ix1 == 1);
    Node test = node_array_get(node_arr, 1);
    check("Array lookup at index", node_is_equal(test, node1) == TRUE);
    node_array_push(&node_arr, node_make_empty());
    node_array_push(&node_arr, node_make_empty());
    node_array_push(&node_arr, node_make_empty());
    size_t cap1 = node_arr.capacity;
    check("Array growth", cap1 == cap0 * GROWTH_FACTOR);
    Node node2 = node_make(Stem, 1, 1337, 0);
    node_array_set(node_arr, 2, node2);
    check("Set node at index",
        node_is_equal(node_array_get(node_arr, 2), node2) == TRUE);

    struct Array spine_buf = array_make(32);
    check("Array is empty",
        array_is_empty(node_arr) == FALSE &&
        array_is_empty(spine_buf) == TRUE);
}

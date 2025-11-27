#include "test.h"

#include "../src/node.h"

#include "../src/debug.h"

void print_node_bits(Node node) {
    char buffer[72];
    sprintbits(buffer, (uint_least64_t)node.left, TRUE);
    printf("%s | ", buffer);
    sprintbits(buffer, (uint_least64_t)node.right, TRUE);
    printf("%s\n", buffer);
}

void test() {
    Node test = node_make(NODE_TYPE_STEM, 1, 0, 16);
    // print_node_bits(test);
    check("Make a stem", node_get_type(test) == NODE_TYPE_STEM);

    test = node_make(NODE_TYPE_FORK, 1, 0, 16);
    check("Make a fork", node_get_type(test) == NODE_TYPE_FORK);

    test = node_make(NODE_TYPE_APP, 1, 16, 17);
    check("Make an app", node_get_type(test) == NODE_TYPE_APP);

    check("Get tag", node_get_type(test) == NODE_TYPE_APP);
    check("Get left child index",
        node_get_child(test, CHILD_SIDE_LEFT) == 16);
    test = node_set_refcount(test, 14);
    test = node_incr_refcount(test);
    test = node_incr_refcount(test);
    test = node_incr_refcount(test);
    test = node_decr_refcount(test);
    test = node_set_child(test, CHILD_SIDE_LEFT, 9);
    test = node_set_child(test, CHILD_SIDE_RIGHT, 9);
    check("Set and get refcount", node_get_refcount(test) == 16);

    char buf[1024];
    node_print(buf, test);
    printf("Node print test: %s\n", buf);

    test = node_set_type(test, NODE_TYPE_INDIR);
    node_print(buf, test);
    printf("Node print test: %s\n", buf);
    check("Set and get indir", node_get_type(test) == NODE_TYPE_INDIR);
}

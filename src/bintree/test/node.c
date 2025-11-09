#include "test.h"

#include "../node.h"

void print_node_bits(Node node) {
    char buffer[72];
    sprintbits(buffer, (uint_least64_t)node.left, TRUE);
    printf("%s | ", buffer);
    sprintbits(buffer, (uint_least64_t)node.right, TRUE);
    printf("%s\n", buffer);
}

void node_test() {
    Node test = node_make(Stem, 1, 16, 0);
    // print_node_bits(test);
    check("Make a stem", node_get_tag(test) == Stem);

    test = node_make(Fork, 1, 0, 16);
    check("Make a fork", node_get_tag(test) == Fork);

    test = node_make(App, 1, 16, 17);
    check("Make an app", node_get_tag(test) == App);

    check("Get tag", node_get_tag(test) == App);
    check("Get left child index", node_get_left_child_index(test) == 16);
    node_set_refcount(&test, 14);
    node_incr_refcount(&test);
    node_incr_refcount(&test);
    node_incr_refcount(&test);
    node_decr_refcount(&test);
    node_set_left_child_index(&test, 9);
    node_set_right_child_index(&test, 9);
    check("Set and get refcount", node_get_refcount(test) == 16);

    char buf[1024];
    node_print(buf, test);
    printf("Node print test: %s\n", buf);

    node_set_tag(&test, Indirection);
    node_print(buf, test);
    printf("Node print test: %s\n", buf);
}

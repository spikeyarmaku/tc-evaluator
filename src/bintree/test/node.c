#include "test.h"

#include "../node.h"

void print_node_bits(Node node) {
#ifdef DEBUG_PRINTS
    char buffer[72];
    for (int i = 0; i < 8; i++) {
        sprintbits(buffer + i * 9, (uint_least64_t)node.data[i], TRUE);
    }
    printf("%s\n", buffer);
#endif
}

void node_test() {
#ifdef DEBUG_PRINTS
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
    check("Set and get refcount", node_get_refcount(test) == 16);

    char buf[1024];
    node_print(buf, test);
    printf("Node print test: %s\n", buf);

    node_set_tag(&test, Indirection);
    node_print(buf, test);
    printf("Node print test: %s\n", buf);
#endif
}

#include "test.h"

#include "../node.h"

#define TAG_MASK_SHIFT 0
#define REFCOUNT_MASK_SHIFT 2
#define LEFT_CHILD_MASK_SHIFT 16
#define RIGHT_CHILD_MASK_SHIFT 40

#define TAG_MASK (((uint_least64_t)1<<2) - 1)
#define REFCOUNT_MASK ((((uint_least64_t)1<<14) - 1) << REFCOUNT_MASK_SHIFT)
#define LEFT_CHILD_MASK ((((uint_least64_t)1<<24) - 1) << LEFT_CHILD_MASK_SHIFT)
#define RIGHT_CHILD_MASK \
    ((((uint_least64_t)1<<24) - 1) << RIGHT_CHILD_MASK_SHIFT)

void print_node_bits(Node node) {
#ifdef DEBUG_PRINTS
    char buffer[72];
    printf("Node: %lu\n", node);
    sprintbits(buffer, (uint_least64_t)node, TRUE);
    printf("%s\n", buffer);
#endif
}

void node_test() {
#ifdef DEBUG_PRINTS
    Node test = node_make(Stem, 1, 16, 0);
    // print_node_bits(test);
    check("Make a stem", test == 1048581);

    test = node_make(Fork, 1, 0, 16);
    check("Make a fork", test == 17592186044422);

    test = node_make(App, 1, 16, 17);
    check("Make an app", test == 18691698720775);

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

    node_set_special_tag(&test, Indirection);
    node_print(buf, test);
    printf("Node print test: %s\n", buf);
#endif
}

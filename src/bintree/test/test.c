#include "test.h"

void node_test();
void array_test();
void tree_test();

void check(char* label, bool_t cond) {
    if (cond == TRUE) {
#ifdef DEBUG_PRINTS
        printf("Test OK - %s\n", label);
#endif
    } else {
        fail("TEST FAILED - %s", label);
    }
}

void main() {
    printf("Node tests\n");
    node_test();

    printf("Array tests\n");
    array_test();

    printf("Tree tests\n");
    tree_test();
}
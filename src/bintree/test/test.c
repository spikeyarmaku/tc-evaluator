#include "test.h"

void node_test();
void array_test();
void tree_test();
void vm_test();

void check(char* label, bool_t cond) {
    if (cond == TRUE) {
        printf("Test OK - %s\n", label);
    } else {
        fail("TEST FAILED - %s", label);
    }
}

int main() {
    printf("Node tests\n");
    node_test();

    printf("Array tests\n");
    array_test();

    printf("Tree tests\n");
    tree_test();

    printf("VM tests\n");
    vm_test();
    return 0;
}

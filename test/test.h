#ifndef TEST_H
#define TEST_H

#include "../src/global.h"
#include <stdlib.h>

void test();

void check(char* label, bool_t cond) {
    if (cond == TRUE) {
        printf("Test OK - %s\n", label);
    } else {
        printf("TEST FAILED - %s", label);
        exit(EXIT_FAILURE);
    }
}

int main() {
    test();
    return 0;
}

#endif

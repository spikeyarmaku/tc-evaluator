#include "debug.h"

#define TAB_SIZE 2

void debug(const char* s, ...) {
    #ifdef DEBUG_PRINTS
    va_list args;
    va_start(args, s);
    vprintf(s, args);
    va_end(args);
    #endif
}

void debug_indent(int indent_amount, const char* s, ...) {
    #ifdef DEBUG_PRINTS
    for (int i = 0; i < indent_amount * TAB_SIZE; i++) {
        printf(" ");
    }
    va_list args;
    va_start(args, s);
    vprintf(s, args);
    va_end(args);
    #endif
}

// Source: https://stackoverflow.com/questions/52845040/printing-a-long-in-binary-64-bit-representation
// buffer: char* array of at least 65 (or 72 if `sep` is true) characters (with
// trailing 0 included)
// n: the number to be printed
// sep: should groups of 8 bits be separated by space
void sprintbits(char* buffer, uint_least64_t n, bool_t sep) {
    uint8_t base_max = sizeof(n) * CHAR_BIT;
    uint8_t base = 0;
    uint8_t cursor = 0;
    uint_least64_t i;
    bool_t trailing = TRUE;
    while(base < base_max) {
        if (sep == TRUE && base % 8 == 0 && base != 0) {
            buffer[cursor] = ' ';
            cursor++;
        }
        i = (uint_least64_t)1 << base;
        if(n & i) {
            buffer[cursor] = '1';
        }
        else {
            buffer[cursor] = '0';
        }
        base++;
        cursor++;
    }
    buffer[cursor] = 0;
}

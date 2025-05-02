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
void printbits(unsigned long n){
    unsigned long i; 
    i = 1UL<<(sizeof(n)*CHAR_BIT-1);
    while(i>0){
         if(n&i)
              printf("1"); 
         else 
              printf("0"); 
         i >>= 1;
    }
}

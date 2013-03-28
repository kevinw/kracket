#include <stdio.h>

#include "binary.h"

#define fixnum_mask     B8(00000011)
#define fixnum_tag      B8(00000000)
#define fixnum_shift    2

#define char_mask       B8(11111111)
#define char_tag        B8(00001111)
#define char_shift      8

#define boolean_mask    B8(01111111)
#define boolean_tag     B8(00111111)
#define boolean_shift   7

#define empty_list      B8(00101111)

int scheme_entry();

int main(int argc, char**argv) {
    int val = scheme_entry();

    if ((val & fixnum_mask) == fixnum_tag) {
        printf("%d\n", val >> fixnum_shift);
    } else if ((val & char_mask) == char_tag) {
        printf("%c\n", val >> char_shift);
    } else if ((val & boolean_mask) == boolean_tag) {
        printf(val >> boolean_shift ? "#t\n" : "#f\n");
    } else if (val == empty_list) {
        printf("'()\n");
    } else {
        printf("got unknown value %d\n", val);
        printBinary(val);
        return 1;
    }

    return 0;
}

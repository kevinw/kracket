#include <stdio.h>

#define fixnum_mask     0b00000011
#define fixnum_tag      0b00000000
#define fixnum_shift    2

#define char_mask       0b11111111
#define char_tag        0b00001111
#define char_shift      8

#define boolean_mask    0b01111111
#define boolean_tag     0b00011111
#define boolean_shift   7

int main(int argc, char**argv) {
    int val = scheme_entry();

    if ((val & fixnum_mask) == fixnum_tag) {
        printf("%d\n", val >> fixnum_shift);
    } else if (val == empty_list) {
        printf("()\n");
    }

    return 0;
}

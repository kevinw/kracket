#include <stdio.h>

/* Helper macros */
#define HEX__(n) 0x##n##LU
#define B8__(x) ((x&0x0000000FLU)?1:0) \
+((x&0x000000F0LU)?2:0) \
+((x&0x00000F00LU)?4:0) \
+((x&0x0000F000LU)?8:0) \
+((x&0x000F0000LU)?16:0) \
+((x&0x00F00000LU)?32:0) \
+((x&0x0F000000LU)?64:0) \
+((x&0xF0000000LU)?128:0)

/* User macros */
#define B8(d) ((unsigned char)B8__(HEX__(d)))
#define B16(dmsb,dlsb) (((unsigned short)B8(dmsb)<<8) \
+ B8(dlsb))
#define B32(dmsb,db2,db3,dlsb) (((unsigned long)B8(dmsb)<<24) \
+ ((unsigned long)B8(db2)<<16) \
+ ((unsigned long)B8(db3)<<8) \
+ B8(dlsb))

#define fixnum_mask     B8(00000011)
#define fixnum_tag      B8(00000000)
#define fixnum_shift    2

#define char_mask       B8(11111111)
#define char_tag        B8(00001111)
#define char_shift      8

#define boolean_mask    B8(01111111)
#define boolean_tag     B8(00011111)
#define boolean_shift   7

#define empty_list      B8(00101111)

int main(int argc, char**argv) {
    int val = scheme_entry();

    if ((val & fixnum_mask) == fixnum_tag) {
        printf("%d\n", val >> fixnum_shift);
    } else if (val == empty_list) {
        printf("()\n");
    }

    return 0;
}

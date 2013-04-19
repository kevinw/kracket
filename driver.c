#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "binary.h"
#include "aux.h"

#define DRIVER_DEBUG 0

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

#define heap_mask       B8(00000111)
#define pair_tag        B8(00000001)
#define vector_tag      B8(00000010)
#define string_tag      B8(00000011)
#define symbol_tag      B8(00000101)
#define closure_tag     B8(00000110)


#define HEAP_SIZE ((size_t)(5 * 1024 * 1024))
#define wordsize sizeof(size_t)

#include <inttypes.h>

#define scheme_val intptr_t

scheme_val scheme_entry();

int is_fixnum(scheme_val val) {
    return (val & fixnum_mask) == fixnum_tag;
}

static scheme_val UNPACK_FIXNUM(scheme_val a) {
    assert(is_fixnum(a));
    return a >> fixnum_shift;
}

void print_value(scheme_val val, int* return_code);

static void print_fixnum(scheme_val val) {
    printf("%" PRIiPTR, UNPACK_FIXNUM(val));
}

static void print_char(scheme_val val) {
    unsigned char c = val >> char_shift;
    if (c == 0)
        printf("#\\nul");
    else
        printf("#\\%c", c);
}

static void print_boolean(scheme_val val) {
    printf(val >> boolean_shift ? "#t" : "#f");
}

static void print_pair(scheme_val val, int* return_code) {
    scheme_val* head = (scheme_val*)(val - 1);
    scheme_val* tail = (scheme_val*)(val - 1 + wordsize);

    printf("(");
    print_value(*head, return_code);
    printf(" . ");
    print_value(*tail, return_code);
    printf(")");
}

static void print_vector(scheme_val val) {
    printf("#(");
    size_t* v = (size_t*)(val & ~heap_mask);
    size_t vectorLength = UNPACK_FIXNUM(*v);
    for (int i = 0; i < vectorLength; ++i) {
        if (i > 0)
            printf(" ");
        printf("%zu", v[i+1]);
    }
    printf(")");
}

static void print_string(scheme_val val) {
    size_t* s = (size_t*)(val & ~heap_mask);
    size_t stringLength = *s;

    const unsigned char* string = (const unsigned char*)(&s[1]);
    printf("\"%.*s\"\n", (int)stringLength, string);
}

void print_value(scheme_val val, int* return_code) {
    if (is_fixnum(val)) {
        print_fixnum(val);
    } else if ((val & char_mask) == char_tag) {
        print_char(val);
    } else if ((val & boolean_mask) == boolean_tag) {
        print_boolean(val);
    } else if (val == empty_list) {
        printf("'()");
    } else if ((val & heap_mask) == pair_tag) {
        print_pair(val, return_code);
    } else if ((val & heap_mask) == vector_tag) {
        print_vector(val);
    } else if ((val & heap_mask) == string_tag) {
        print_string(val);
    } else {
        printf("got unknown value %zu: ", val);
        printBinary(val);
        *return_code = 1;
    }
}

int main(int argc, char**argv) {
    void* heap;
    if (!(heap = malloc(HEAP_SIZE))) {
        fprintf(stderr, "could not pre-allocate heap");
        return 1;
    }

#if DRIVER_DEBUG
    printf("HEAP PTR %p\n", heap);
#endif

    scheme_val val = scheme_entry(heap);

    int return_code = 0;
    print_value(val, &return_code);
    printf("\n");

    free(heap);

    return return_code;
}

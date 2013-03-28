#include <stdio.h>
#include <stdlib.h>

#include "aux.h"

static void strreverse(char* begin, char* end) {
	char aux;
	while (end > begin)
		aux=*end, *end--=*begin, *begin++=aux;
}
	
static void itoa(int value, char* str, int base) {
	static char num[] = "0123456789abcdefghijklmnopqrstuvwxyz";
	char* wstr=str;
	int sign;
	div_t res;

	// Validate base
	if (base<2 || base>35){ *wstr='\0'; return; }

	// Take care of sign
	if ((sign=value) < 0) value = -value;

	// Conversion. Number is reversed.
	do {
		res = div(value,base);
		*wstr++ = num[res.rem];
	} while(value=res.quot);
	if (sign < 0) *wstr++='-';
	*wstr = '\0';

	// Reverse string
	strreverse(str, wstr - 1);
}

void printBinary(int i) {
  char buffer[33];
  itoa(i, buffer, 2);
  printf("%s\n", buffer);
}


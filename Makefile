CC=gcc

output.s: input.scm
	mzscheme compiler.scm input.scm > output.s

a.out: output.s driver.c
	$(CC) -O3 output.s driver.c


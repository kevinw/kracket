CC=gcc

SCHEME=mzscheme

CFLAGS=-g -O3
CFLAGS32=-g -O3 -arch i386

BIN=a64.out
ASSEMBLY=output64.s
ASSEMBLYOBJ=output64.o

BIN32=a32.out
ASSEMBLY32=output32.s
ASSEMBLYOBJ32=output32.o


all: $(ASSEMBLY) $(BIN)

all32: $(ASSEMBLY32) $(BIN32)

$(ASSEMBLY): compiler.rkt input.rkt assembler.rkt
	$(SCHEME) compiler.rkt input.rkt $@

$(ASSEMBLYOBJ): $(ASSEMBLY)
	$(CC) $(CFLAGS) $^ -c

$(ASSEMBLY32): compiler.rkt input.rkt assembler.rkt
	$(SCHEME) compiler.rkt input.rkt $@ x86

$(ASSEMBLYOBJ32): $(ASSEMBLY32)
	$(CC) $(CFLAGS32) $^ -c

$(BIN32): $(ASSEMBLYOBJ32) driver.c aux.c
	$(CC) $(CFLAGS32) $^ -o $(BIN32)

$(BIN): $(ASSEMBLYOBJ) driver.c aux.c
	$(CC) $(CFLAGS) $^ -o $(BIN)

.PHONY: clean run test run32

clean: 
	rm -f $(ASSEMBLY) $(ASSEMBLY32) $(BIN) $(BIN32)

run: all
	./$(BIN)

run32: all32
	./$(BIN32)

test:
	$(SCHEME) test-cases.rkt

CC=gcc

SCHEME=mzscheme

CFLAGS=-std=c99 -Wall -g -O3
CFLAGS32=$(CFLAGS) -arch i386

LDFLAGS=-Wl,-no_pie
LDFLAGS32=$(LDFLAGS)

BIN=a64.out
ASSEMBLY=output64.s
ASSEMBLYOBJ=output64.o
DRIVEROBJ32=driver32.o

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
	$(CC) $(CFLAGS32) $^ -c -o $(ASSEMBLYOBJ32)

$(DRIVEROBJ32): driver.c
	$(CC) $(CFLAGS32) $^ -c -o $(DRIVEROBJ32)

$(BIN32): $(ASSEMBLYOBJ32) $(DRIVEROBJ32) aux.c
	$(CC) $(CFLAGS32) $(LDFLAGS32) $^ -o $(BIN32)

$(BIN): $(ASSEMBLYOBJ) driver.c aux.c
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $(BIN)

.PHONY: clean run test run32

clean: 
	rm -f $(ASSEMBLY) $(ASSEMBLY32) $(ASSEMBLYOBJ) $(ASSEMBLYOBJ32) $(BIN) $(BIN32) $(DRIVEROBJ32)

run: all
	./$(BIN)

debug: all
	gdb ./$(BIN)

run32: all32
	./$(BIN32)

debug32: all32
	gdb ./$(BIN32)

test:
	$(SCHEME) test-cases.rkt

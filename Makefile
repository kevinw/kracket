CC=gcc

SCHEME=mzscheme

CFLAGS=-g -O3

BIN=a64.out
ASSEMBLY=output64.s

BIN32=a32.out
ASSEMBLY32=output32.s

all: $(ASSEMBLY) $(BIN)

all32: $(ASSEMBLY32) $(BIN32)

$(ASSEMBLY): compiler.rkt input.rkt
	$(SCHEME) compiler.rkt input.rkt $@

$(ASSEMBLY32): compiler.rkt input.rkt
	$(SCHEME) compiler.rkt input.rkt $@ x86

$(BIN32): $(ASSEMBLY32) driver.c aux.c
	$(CC) $(CFLAGS) -arch i386 $^ -o $(BIN32)

$(BIN): $(ASSEMBLY) driver.c aux.c
	$(CC) $(CFLAGS) $^ -o $(BIN)

.PHONY: clean run test run32

clean: 
	rm -f $(ASSEMBLY) $(ASSEMBLY32) $(BIN) $(BIN32)

run: all
	lipo -info ./$(BIN)
	./$(BIN)

run32: all32
	lipo -info ./$(BIN32)
	./$(BIN32)

test:
	$(SCHEME) test-cases.rkt

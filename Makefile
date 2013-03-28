CC=gcc

SCHEME=mzscheme
BIN=a.out
ASSEMBLY=output.s

all: $(ASSEMBLY) $(BIN)

$(ASSEMBLY): compiler.rkt input.rkt
	$(SCHEME) compiler.rkt input.rkt $@

$(BIN): $(ASSEMBLY) driver.c aux.c
	$(CC) -O3 $^ -o $(BIN)

.PHONY: clean run test

clean: 
	rm -f $(ASSEMBLY) $(BIN)

run: all
	./$(BIN)

test:
	$(SCHEME) test-cases.rkt

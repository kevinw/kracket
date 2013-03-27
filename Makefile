CC=gcc

BIN=a.out
ASSEMBLY=output.s

all: $(ASSEMBLY) $(BIN)

$(ASSEMBLY): input.scm
	mzscheme compiler.scm input.scm $@

$(BIN): $(ASSEMBLY) driver.c
	$(CC) -O3 $^

.PHONY: clean run

clean: 
	rm -f $(ASSEMBLY) $(BIN)

run: all
	./a.out

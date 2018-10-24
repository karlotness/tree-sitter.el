CC?=gcc
CFLAGS+=-std=c99 -Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2
LDFLAGS+=

# Depend on the tree-sitter parser C file and the Emacs module source
sources=src/parser.c lang-module.c

all: tree-sitter-lang-LANG_NAME-module.so

tree-sitter-lang-LANG_NAME-module.so: $(sources:.c=.o)
	$(CC) -shared $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) -fPIC -c $(CFLAGS) -o $@ $<

clean:
	rm -f src/parser.o lang-module.o tree-sitter-lang-LANG_NAME-module.so

.PHONY: clean

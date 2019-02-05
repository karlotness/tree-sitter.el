CC?=gcc
CFLAGS+=-std=c99 -Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2 -I../../externals/tree-sitter/lib/include \
    -I../../includes/
LDFLAGS+=

# Depend on the tree-sitter parser C file and the Emacs module source
sources_c=externals/tree-sitter-c/src/parser.c $(wildcard *.c)

include version.mk

all: dist

version.mk: tree-sitter-lang-c-pkg.el
	@sed -n 's/(define-package ".*" "\([0-9\.]*\)"/VERSION=\1/p' $< > version.mk

tree-sitter-lang-c-module.so: $(sources_c:.c=.o)
	$(CC) -shared -fPIC $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) -fPIC -c $(CFLAGS) -o $@ $<

dist: tree-sitter-lang-c-$(VERSION).tar

tree-sitter-lang-c-%.tar: tree-sitter-lang-c-module.so $(wildcard *.el)
	mkdir "tree-sitter-lang-c-$*"
	cp $^ "tree-sitter-lang-c-$*"
	tar -cf $@ "tree-sitter-lang-c-$*"
	rm -r "tree-sitter-lang-c-$*"

clean:
	rm -f $(sources_c:.c=.o) tree-sitter-lang-c-module.so

submod:
	git submodule update --init externals/tree-sitter-c

.PHONY: clean submod dist

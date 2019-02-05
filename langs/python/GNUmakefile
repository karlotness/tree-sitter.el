CC?=gcc
CFLAGS+=-std=c99 -Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2 -I../../externals/tree-sitter/lib/include \
    -I../../includes/
CXXFLAGS+=-Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2 -I../../externals/tree-sitter/lib/include \
    -I../../includes/
LDFLAGS+=

# Depend on the tree-sitter parser C file and the Emacs module source
sources_c=externals/tree-sitter-python/src/parser.c $(wildcard *.c)
sources_cc=externals/tree-sitter-python/src/scanner.cc

include version.mk

all: dist

version.mk: tree-sitter-lang-python-pkg.el
	@sed -n 's/(define-package ".*" "\([0-9\.]*\)"/VERSION=\1/p' $< > version.mk

tree-sitter-lang-python-module.so: $(sources_c:.c=.o) $(sources_cc:.cc=.o)
	$(CC) -shared -fPIC $(LDFLAGS) -o $@ $^

%.o: %.cc
	$(CC) -fPIC -c $(CXXFLAGS) -o $@ $<

%.o: %.c
	$(CC) -fPIC -c $(CFLAGS) -o $@ $<

dist: tree-sitter-lang-python-$(VERSION).tar

tree-sitter-lang-python-%.tar: tree-sitter-lang-python-module.so $(wildcard *.el)
	mkdir "tree-sitter-lang-python-$*"
	cp $^ "tree-sitter-lang-python-$*"
	tar -cf $@ "tree-sitter-lang-python-$*"
	rm -r "tree-sitter-lang-python-$*"

clean:
	rm -f $(sources_c:.c=.o) $(sources_cc:.cc=.o) tree-sitter-lang-python-module.so

submod:
	git submodule update --init externals/tree-sitter-python

.PHONY: clean submod dist

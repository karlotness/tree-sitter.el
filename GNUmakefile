# Copyright (C) 2018 Karl Otness
#
# This file is part of tree-sitter.el.
#
# tree-sitter.el is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# tree-sitter.el is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tree-sitter.el. If not, see
# <https://www.gnu.org/licenses/>.
CC?=gcc
CFLAGS+=-std=c99 -O2 -Wall -Wextra -Wpedantic -Iexternals/tree-sitter/lib/include \
  -Iincludes/

sources=$(wildcard src/*.c)

include version.mk

all: dist

-include $(sources:.c=.d)

version.mk: lisp/tree-sitter-pkg.el
	@sed -n 's/(define-package ".*" "\([0-9\.]*\)"/VERSION=\1/p' lisp/tree-sitter-pkg.el > version.mk

tree-sitter-module.so: $(sources:.c=.o) externals/tree-sitter/libtree-sitter.o
	$(CC) -shared -fPIC -o $@ $^

# Build step derived from tree-sitter's "build-lib" script.
externals/tree-sitter/libtree-sitter.o: externals/tree-sitter/lib/utf8proc/utf8proc.c \
  externals/tree-sitter/lib/utf8proc/utf8proc.h \
  $(wildcard externals/tree-sitter/lib/src/*.c) \
  $(wildcard externals/tree-sitter/lib/include/tree_sitter/*)
	$(CC) -c -fPIC -O3 -std=c99 -Iexternals/tree-sitter/lib/src \
	      -Iexternals/tree-sitter/lib/include \
	      -Iexternals/tree-sitter/lib/utf8proc \
	      externals/tree-sitter/lib/src/lib.c \
	      -o $@

dist: tree-sitter-$(VERSION).tar

tree-sitter-%.tar: tree-sitter-module.so $(wildcard lisp/*.el)
	mkdir "tree-sitter-$*"
	cp $^ "tree-sitter-$*"
	tar -cf $@ "tree-sitter-$*"
	rm -r "tree-sitter-$*"

%.o: %.c
	$(CC) $(CFLAGS) -fPIC -c -o $@ $<

# Rule taken from GNU Make manual
%.d: %.c
	@set -e; rm -f $@; \
	$(CC) -M -MT $(<:.c=.o) $(CFLAGS) $< 2> /dev/null > $@.$$$$; \
	sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
	rm -f $@.$$$$

submod:
	git submodule update --init externals/tree-sitter
	cd externals/tree-sitter && git submodule update --init lib/utf8proc

clean:
	rm -f src/*.o src/*.d src/*.d.*
	rm -f tree-sitter-module.so
	rm -f externals/tree-sitter/libtree-sitter.o
	rm -f version.mk $(wildcard tree-sitter-*.tar.gz)

.PHONY: clean dist submod

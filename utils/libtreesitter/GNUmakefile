CC?=gcc
LIBDIR?=/usr/local/lib/
INCLUDEDIR?=/usr/local/include/
CFLAGS+=-std=c99 -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2\
	-Iinclude -Isrc -Iexternals/utf8proc
LDFLAGS+=

sources=$(wildcard src/runtime/*.c)

all: libtreesitter.so

libtreesitter.so: $(sources:.c=.o) externals/utf8proc/utf8proc.o
	$(CC) -shared $(LDFLAGS) -o $@ $^

%.o: %.c
	$(CC) -fPIC -c $(CFLAGS) -o $@ $<

submod:
	git submodule update --init "externals/utf8proc"

install: libtreesitter.so
	install -m 755 -D -t "$(LIBDIR)" libtreesitter.so
	install -m 644 -D -t "$(INCLUDEDIR)/tree_sitter" include/tree_sitter/runtime.h

clean:
	rm -f src/runtime/*.o src/runtime/*.d src/runtime/*.d.*
	rm -f externals/utf8proc/*.o externals/utf8proc/*.d externals/utf8proc/*.d.*
	rm -f libtreesitter.so

.PHONY: clean submod install

CC?=gcc
CFLAGS+=-std=c99 -Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2
CXXFLAGS+=-Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2
LDFLAGS+=

# Depend on the tree-sitter parser C file and the Emacs module source
sources_c=$(wildcard src/*.c) lang-module.c
remove_sources_cc=src/binding.cc
sources_cc=$(filter-out $(remove_sources_cc), $(wildcard src/*.cc))

all: tree-sitter-lang-LANG_NAME-module.so

tree-sitter-lang-LANG_NAME-module.so: $(sources_c:.c=.o) $(sources_cc:.cc=.o)
	$(CC) -shared $(LDFLAGS) -o $@ $^

%.o: %.cc
	$(CC) -fPIC -c $(CXXFLAGS) -o $@ $<

%.o: %.c
	$(CC) -fPIC -c $(CFLAGS) -o $@ $<

clean:
	rm -f $(sources_c:.c=.o) $(sources_cc:.cc=.o) tree-sitter-lang-LANG_NAME-module.so

.PHONY: clean

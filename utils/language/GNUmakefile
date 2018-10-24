CC?=gcc
CFLAGS+=-std=c99 -Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2
CXXFLAGS+=-Isrc/ -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1 -O2
LDFLAGS+=

# Depend on the tree-sitter parser C file and the Emacs module source
sources_c=src/parser.c lang-module.c
sources_cc=src/scanner.cc

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

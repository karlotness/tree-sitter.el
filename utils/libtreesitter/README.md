# Building the tree-sitter runtime
The GNU makefile in this directory is builds the [tree-sitter][1]
runtime as a shared object. The steps involved are based on the
tree-sitter node-gyp build process, but modified to produce a
relocatable shared object rather than a statically linked library.

## Setup
The main steps:
1. Clone the tree-sitter [repository][1]
2. Place the `GNUmakefile` from this directory into the `tree-sitter`
   root directory
3. Initialize the `externals/utf8proc` submodule
   - One approach: run `make submod` which will do this step
4. Build `libtreesitter.so` by running `make`
5. Install the shared object and headers
   - `make install` will do this for you
   - You can customize `LIBDIR` and `INCLUDEDIR` for the shared object
     and headers, respectively.


To customize the install paths, override the default locations when
invoking `make install`:
```
make LIBDIR="/usr/lib/dest/here/" INCLUDEDIR="/usr/include/dest/here/" install
```
which will specify the prefixes for the installed files. The
installation process will place `libtreesitter.so` under `LIBDIR` and
runtime.h at `INCLUDEDIR/tree_sitter`.

[1]: https://github.com/tree-sitter/tree-sitter

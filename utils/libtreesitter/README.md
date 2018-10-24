# Building the tree-sitter runtime
The GNU makefile in this directory is builds the [tree-sitter][1]
runtime as a shared object. The steps involved are based on the
tree-sitter node-gyp build process, but modified to produce a
relocatable shared object rather than a statically linked library.

To build tree-sitter using the makefile, first clone the tree-sitter
[repository][1]. Next, the tree-sitter runtime requires the
`externals/utf8proc` submodule. Unless you have particular
requirements for how these modules should be fetched (for example, if
you are packaging the runtime) you can run `make submod` to clone this
submodule.

Next, build `libtreesitter.so` by running `make`. Finally, you can
install the runtime shared object and headers with `make install`. If
you would like to direct these files to an alternate location run this
as:
```
make LIBDIR="/usr/lib/dest/here/" INCLUDEDIR="/usr/include/dest/here/" install
```
which will specify the prefixes for the installed files.

The installation process will place `libtreesitter.so` under `LIBDIR`
and runtime.h at `INCLUDEDIR/tree_sitter`.

[1]: https://github.com/tree-sitter/tree-sitter

# tree-sitter.el
An Emacs dynamic module exposing [tree-sitter][1].

The build requires the `emacs-module.h` header in a system location.
The installation process for Emacs 26 and above will correctly place
the module header, otherwise it will need to be added manually.

## Status
A list of functions and information on whether they have been exposed
to Elisp is available in [todo.org](todo.org).

The bindings are relatively plain transfers of the tree-sitter C
API. Few, if any, convenience wrappers are provided although some may
be added later. That said, the intention is that all types and
functions provided by the module should be safe to call and they
should not crash Emacs. The values returned by the module are also
intended to be garbage collected by Emacs, so no manual memory
management functions from tree-sitter will be directly exposed.

The interface to the module is *not settled*. The binding code is in
early stages and will need a fair bit of cleanup to fix
inconsistencies and patch likely bugs. Function names and signatures
may change in a later pass. The interface is currently verbose and
ideally the names will be compacted and the interface cleaned up in
the future.

## Installation
Building the package requires a few external dependencies. These are
included in this repository as Git submodules. To build, first clone
the repository and change to the root directory. Then run
```sh
make submod
make dist
```
The first step will recursively initialize the submodules and the
second will produce a tar file with the package. Install that in Emacs
using `package-install-file`.

### Language grammar
To make any real use of the module you will also need a language
grammar for tree-sitter. Several of these are provided by tree-sitter
and are listed under its [project page][2]. A few of these have
packages under the `langs/` directory in this repository. To build one
of these, change to the directory of that language and run
```sh
make submod
make dist
```
Just like for the binding package, the first step will initialize the
submodules, and the second will produce a tar file with the package.
Install that with `package-install-file` in Emacs.

**Note:** you will first need to run `make submod` at the root of this
repository so that the tree-sitter headers are available.

To package other language grammars you might re-purpose the file for
one of these languages to build against a different tree-sitter
grammar.

## Usage
Once you have the module installed you can load it by requiring
`tree-sitter`. Also be sure to require your language.
```elisp
(require 'tree-sitter)
(require 'tree-sitter-lang-python)
```
The functions exposed follow the names of their tree-sitter C
counterparts, prefixed with `tree-sitter` rather than `ts`. For a
basic introduction to using tree-sitter see the project
[documentation][3].

You can also configure live parsing by first adding your language
grammar to `tree-sitter-live-auto-alist` and then enabling
`global-tree-sitter-live-mode`. For example:
```elisp
(setq tree-sitter-live-auto-alist
      '((python-mode . tree-sitter-lang-python)))
(global-tree-sitter-live-mode t)
```

## License
Note that the license, as described below, applies only to the code
contained directly within this repository. Code contained within
submodules, such as those contained in `externals/` directories is
licensed separately. Check the license of these other files before
using them.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

Please see [LICENSE.txt](LICENSE.txt) for a copy of the license.

[1]: https://github.com/tree-sitter/tree-sitter
[2]: https://github.com/tree-sitter
[3]: http://tree-sitter.github.io/tree-sitter/

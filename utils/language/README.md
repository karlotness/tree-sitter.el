# Binding language parsers
A skeleton for making new TSLanguage values available to Emacs. The
files in this directory are configured to get a makeshift module built
quickly from one of the language grammars distributed by the
[tree-sitter project][1]. The setup is not terribly clean, but it
should produce a module which can be loaded and used with
`tree-sitter.el`.

# Setup
The main tasks you will need to perform are:
1. Clone a language repository (such as [python][2])
2. Copy the files from this directory into the root of the language
   grammar
3. Replace all instances of `LANGUAGE_NAME` with the name of your
   binding
   - Some will use underscores and some dashes
   - Also make this replacement in the file names
   - For example for Python you would end up with the file
     `tree-sitter-lang-python.el`
   - You might find grep helpful: `grep --color 'LANG_NAME' *`
4. Uncomment the `plugin_is_GPL_compatible` in `lang-module.c`
   - Only do this if you are sure that your binding and code all have
     GPL-compatible licenses.
5. Build the module with `make`

[1]: https://github.com/tree-sitter/
[2]: https://github.com/tree-sitter/tree-sitter-python.git

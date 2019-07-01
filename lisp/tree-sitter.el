;;; tree-sitter.el --- Emacs bindings to the tree-sitter parser  -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019 Karl Otness

;; This file is part of tree-sitter.el.

;; tree-sitter.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; tree-sitter.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with tree-sitter.el. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; tree-sitter.el is a set of Emacs lisp bindings to the tree-sitter
;; parsing library (https://github.com/tree-sitter/tree-sitter). These
;; are provided as an Emacs dynamic module and a set of Emacs lisp
;; support files.

;;; Code:
(require 'tree-sitter-defs)
(require 'tree-sitter-module)

(defun tree-sitter-language-fields (lang)
  "Retrieve fields in a tree sitter language.
Returns a list of fields and their names for tree-sitter-language
LANG. The list contains (NAME . FIELD) pairs where NAME is a
string and FIELD is the tree-sitter-field record."
  (let ((fields nil)
        (count (tree-sitter-language-field-count lang)))
    (dotimes (i count)
      (let* ((id (1+ i))
             (field (tree-sitter-field--create id))
             (name (tree-sitter-language-field-name lang field)))
        (push (cons name field) fields)))
    (nreverse fields)))

(defun tree-sitter-language-symbols (lang &optional type)
    "Retrieve symbols for a tree sitter language.
Returns a list of symbols and their names for
tree-sitter-language LANG. The list contains (NAME . SYMBOL)
pairs where NAME is a string and SYMBOL is the tree-sitter-symbol
record.

The optional argument TYPE allows filtering the list of symbols.
When unspecified or nil all symbols will be included. When TYPE
is another symbol only those symbols with a matching value under
`tree-sitter-language-symbol-type' will be included."
  (let ((symbols nil)
        (count (tree-sitter-language-symbol-count lang)))
    (dotimes (i count)
      (let* ((id i)
             (symbol (tree-sitter-symbol--create id))
             (name (tree-sitter-language-symbol-name lang symbol)))
        (when (or (not type) (eq type
                                 (tree-sitter-language-symbol-type lang symbol)))
          (push (cons name symbol) symbols))))
    (nreverse symbols)))

(provide 'tree-sitter)
;;; tree-sitter.el ends here

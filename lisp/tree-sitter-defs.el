;;; tree-sitter.el --- Common definitions for tree-sitter.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Karl Otness

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

;; Common definitions used in tree-sitter.el. These are `cl-defstruct'
;; definitions and the like.

;;; Code:
(require 'cl-macs)

(cl-defstruct tree-sitter-language ptr)
(cl-defstruct tree-sitter-parser ptr)
(cl-defstruct tree-sitter-tree ptr)
(cl-defstruct tree-sitter-node ptr)

(provide 'tree-sitter-defs)
;;; tree-sitter-defs.el ends here

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

;; Common definitions used in tree-sitter.el.

;;; Code:

(defun tree-sitter-language--create (ptr)
  "Create a new tree-sitter-language record.
Users should not call this function."
  (record 'tree-sitter-language ptr))

(defun tree-sitter-symbol--create (code)
  "Create a new tree-sitter-symbol record.
Users should not call this function."
  (record 'tree-sitter-symbol code))

(defun tree-sitter-parser--create (ptr)
  "Create a new tree-sitter-parser record.
Users should not call this function."
  (record 'tree-sitter-parser ptr))

(defun tree-sitter--coerce-byte (buf byte-pos)
  "Coerce a byte BYTE-POS into a valid buffer location within BUF.
Users should not call this function."
  (with-current-buffer buf
    (cond ((< byte-pos 0) (point-min))
          ((byte-to-position (1+ byte-pos)) (byte-to-position (1+ byte-pos)))
          (t (save-restriction
               (widen)
               (point-max))))))

(defun tree-sitter--buffer-substring (buf byte-pos read-len)
  "Return a substring from buffer BUF starting a BYTE-POS for length READ-LEN.
Users should not call this function."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let* ((disable-point-adjustment t)
             (start (tree-sitter--coerce-byte buf byte-pos))
             (end (tree-sitter--coerce-byte buf (+ byte-pos read-len))))
        (buffer-substring start end)))))

(provide 'tree-sitter-defs)
;;; tree-sitter-defs.el ends here

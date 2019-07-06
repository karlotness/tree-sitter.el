;;; tree-sitter-defs.el --- Common definitions for tree-sitter.el  -*- lexical-binding: t; -*-

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

(defun tree-sitter-field--create (code)
    "Create a new tree-sitter-field record.
Users should not call this function."
    (record 'tree-sitter-field code))

(defun tree-sitter-parser--create (ptr)
  "Create a new tree-sitter-parser record.
Users should not call this function."
  (record 'tree-sitter-parser ptr))

(defun tree-sitter-tree--create (ptr)
  "Create a new tree-sitter-tree record.
Users should not call this function."
  (record 'tree-sitter-tree ptr))

(defun tree-sitter-node--create (ptr)
  "Create a new tree-sitter-tree record.
Users should not call this function."
  (record 'tree-sitter-node ptr))

(defun tree-sitter-point--create (row col)
  "Create a new tree-sitter-point record.
Users should not call this function."
  (record 'tree-sitter-point row col))

(defun tree-sitter-point-row (point)
  "Return the row of a tree-sitter-point record, POINT."
  (aref point 1))

(defun tree-sitter-point-col (point)
  "Return the column of a tree-sitter-point record, POINT."
  (aref point 2))

(defun tree-sitter-position-to-point (&optional position)
  "Convert a buffer location POSITION to a tree-sitter-point record.
The row and column numbers computed are absolute. If POSITION is
unspecified, use `point'"
  (let ((position (or position (point))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char position)
        (let* ((buffer-invisibility-spec nil)
               (row (line-number-at-pos))
               (col (current-column)))
          (tree-sitter-point--create row col))))))

(defun tree-sitter--coerce-byte (buf byte-pos)
  "Coerce a byte BYTE-POS into a valid buffer location within BUF.
Users should not call this function."
  (with-current-buffer buf
    (cond ((< byte-pos 0) (point-min))
          ((byte-to-position byte-pos) (byte-to-position byte-pos))
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
        (buffer-substring-no-properties start end)))))

(defun tree-sitter-range--create (start-point end-point start-byte end-byte)
  "Create a new tree-sitter-range record.
Users should not call this function."
  (record 'tree-sitter-range start-point end-point start-byte end-byte))

(defun tree-sitter-range-start-point (range)
  "Return the start point of a tree-sitter-range record, RANGE."
  (aref range 1))

(defun tree-sitter-range-end-point (range)
  "Return the end point of a tree-sitter-range record, RANGE."
  (aref range 2))

(defun tree-sitter-range-start-byte (range)
  "Return the start byte of a tree-sitter-range record, RANGE."
  (aref range 3))

(defun tree-sitter-range-end-byte (range)
  "Return the end byte of a tree-sitter-range record, RANGE."
  (aref range 4))

(provide 'tree-sitter-defs)
;;; tree-sitter-defs.el ends here

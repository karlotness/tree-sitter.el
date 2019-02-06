;;; tree-sitter-live-preview.el --- Preview tree-sitter trees  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Karl Otness

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

;; Preview trees produced by tree-sitter for buffers in
;; `tree-sitter-live-mode'. The previews produced by this function can
;; be slow to produce so be careful when using it on large buffers.
;; Previews are inserted into a new buffer by
;; `tree-sitter-live-preview'.

;;; Code:
(require 'tree-sitter-live)

(defvar-local tree-sitter-live-preview--buffer nil)

(defun tree-sitter-live-preview--shorten (text)
  (let ((one-line (mapconcat (lambda (c) (if (eql ?\n c) " " (string c)))
                             text "")))
    (if (> (length one-line) 27)
        (let ((start (substring one-line 0 12))
              (end (substring one-line -12 nil)))
          (format "%s...%s" start end))
      one-line)))

(defun tree-sitter-live-preview--revert (ignore-auto noconfirm)
  (if (buffer-live-p tree-sitter-live-preview--buffer)
      (with-current-buffer tree-sitter-live-preview--buffer
        (tree-sitter-live-preview))
    (error "Source buffer is dead")))

(defun tree-sitter-live-preview--format (node)
  (let ((name (tree-sitter-node-type node)))
    name))

(defun tree-sitter-live-preview--node (node parent-markers)
  (let* ((name (tree-sitter-node-type node))
         (next-sibling (tree-sitter-node-next-sibling node))
         (next-child (tree-sitter-node-child node 0))
         (prefix (mapconcat 'identity
                            (nreverse (cons (cond ((not parent-markers) "")
                                                  (next-sibling "├ ")
                                                  (t "└ "))
                                            (mapcar (lambda (b) (if b "│ " "  "))
                                                    parent-markers)))
                            "")))
    (with-current-buffer tree-sitter-live-preview--buffer
      (insert prefix (tree-sitter-live-preview--format node) "\n"))
    (when next-child
      (tree-sitter-live-preview--node next-child (cons next-sibling parent-markers)))
    (when next-sibling
      (tree-sitter-live-preview--node next-sibling parent-markers))))

;;;###autoload
(defun tree-sitter-live-preview ()
  "Preview tree-sitter tree for the current buffer.

This requires `tree-sitter-live-mode' to be enabled for this
buffer."
  (interactive)
  (unless tree-sitter-live-tree
    (error "No tree-sitter tree for this buffer"))
  (let ((tree-buf
         (or (when (buffer-live-p tree-sitter-live-preview--buffer)
               tree-sitter-live-preview--buffer)
             (get-buffer-create (format "ts-tree: %s" (buffer-name)))))
        (inhibit-read-only t))
    (setq tree-sitter-live-preview--buffer tree-buf)
    (let ((source (current-buffer)))
      (with-current-buffer tree-buf
        (erase-buffer)
        (special-mode)
        (read-only-mode 1)
        (setq-local revert-buffer-function #'tree-sitter-live-preview--revert)
        (setq tree-sitter-live-preview--buffer source)))
    (tree-sitter-live-preview--node
     (tree-sitter-tree-root-node tree-sitter-live-tree) nil)
    (with-current-buffer tree-buf
      (goto-char (point-min)))
    (display-buffer tree-buf)))

(provide 'tree-sitter-live-preview)
;;; tree-sitter-live-preview.el ends here

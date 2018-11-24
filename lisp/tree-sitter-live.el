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

;; Live incremental parsing for buffers. Enable in a buffer using
;; `tree-sitter-live-setup'. Buffers are parsed using tree-sitter
;; after Emacs is idle for the number of seconds specified by
;; `tree-sitter-live-idle-time'. After the buffer is re-parsed, the
;; hook `tree-sitter-live-after-parse-hook' is run with the affected
;; buffer as current. The updated tree is stored in
;; `tree-sitter-live-tree'.

;;; Code:
(require 'tree-sitter-defs)
(require 'tree-sitter-module)

(defgroup tree-sitter-live nil
  "Options controlling live parsing of buffers with tree-sitter.")

(defcustom tree-sitter-live-idle-time 0.25
  "Idle time in seconds before re-parsing buffers with tree-sitter."
  :type 'float
  :group 'tree-sitter-live)
(defcustom tree-sitter-live-after-parse-functions nil
  "Functions to call after a buffer is re-parsed with tree-sitter.
The affected buffer is current while this hook is running.
Functions are called with one argument: the old tree from before
the most recent re-parse. The current tree is stored in the
variable `tree-sitter-live-tree'.

Note that after the initial parse of the buffer, the old tree
value provided to these functions will be nil."
  :type 'hook
  :group 'tree-sitter-live)

(defvar tree-sitter-live-auto-alist nil
  "Alist specifying tree-sitter languages by major mode symbols.
Each entry is a pair of (MODE . LANG) where MODE is a major-mode
symbol and LANG is a tree-sitter language")

(defvar tree-sitter-live--idle-timer nil
  "Idle timer for tree-sitter-live.")
(defvar tree-sitter-live--pending-buffers nil
  "List of buffers which need to be re-parsed at next idle interval.")

(defvar-local tree-sitter-live--parser nil
  "Tree-sitter parser used to parse this buffer.")
(defvar-local tree-sitter-live-tree nil
  "Tree-sitter tree for the current buffer.")
;; Store [start_byte old_end_byte start_point old_end_point]
(defvar-local tree-sitter-live--before-change nil
  "Internal value for tracking old buffer locations")

(defun tree-sitter-live--before-change (beg end)
  "Hook for `before-change-functions'."
  (save-excursion
    (let ((start-byte (position-bytes beg))
          (old-end-byte (position-bytes end))
          (start-point (tree-sitter-position-to-point beg))
          (old-end-point (tree-sitter-position-to-point end)))
      (aset tree-sitter-live--before-change 0 start-byte)
      (aset tree-sitter-live--before-change 1 old-end-byte)
      (aset tree-sitter-live--before-change 2 start-point)
      (aset tree-sitter-live--before-change 3 old-end-point))))

(defun tree-sitter-live--after-change (beg end pre-len)
  "Hook for `after-change-functions'."
  (let ((start-byte (aref tree-sitter-live--before-change 0))
        (old-end-byte (aref tree-sitter-live--before-change 1))
        (new-end-byte (position-bytes end))
        (start-point (aref tree-sitter-live--before-change 2))
        (old-end-point (aref tree-sitter-live--before-change 3))
        (new-end-point (tree-sitter-position-to-point end)))
    (tree-sitter-tree-edit tree-sitter-live-tree
                           start-byte old-end-byte new-end-byte
                           start-point old-end-point new-end-point)
    (when (not (memq (current-buffer) tree-sitter-live--pending-buffers))
      (push (current-buffer) tree-sitter-live--pending-buffers))))

(defun tree-sitter-live--idle-update ()
  (dolist (buf tree-sitter-live--pending-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((old-tree tree-sitter-live-tree))
          (setq tree-sitter-live-tree
                (tree-sitter-parser-parse-buffer tree-sitter-live--parser
                                                 (current-buffer)
                                                 tree-sitter-live-tree))
        (run-hook-with-args 'tree-sitter-live-after-parse-functions old-tree)))))
  (setq tree-sitter-live--pending-buffers nil))

(defun tree-sitter-live--auto-language ()
  (catch 'tree-sitter-live--auto-language
    (dolist (lp tree-sitter-live-auto-alist)
      (let ((mode (car lp))
            (lang (cdr lp)))
        (when (derived-mode-p mode)
          (throw 'tree-sitter-live--auto-language lang))))
    nil))

(defun tree-sitter-live-setup (&optional language)
  "Enable tree-sitter-live for LANGUAGE in current buffer.
LANGUAGE must be a tree-sitter-language record. If LANGUAGE is
unspecified consult `tree-sitter-live-auto-alist' for the
language to use based on `major-mode'."
  (setq tree-sitter-live--parser (tree-sitter-parser-new))
  (let ((language (or language (tree-sitter-live--auto-language))))
    (unless language
      (error "Language unspecified for tree-sitter-live"))
    (tree-sitter-parser-set-language tree-sitter-live--parser language))
  (setq tree-sitter-live-tree
        (tree-sitter-parser-parse-buffer tree-sitter-live--parser
                                         (current-buffer)))
  (run-hook-with-args 'tree-sitter-live-after-parse-functions nil)
  (setq tree-sitter-live--before-change (make-vector 4 0))
  (add-hook 'before-change-functions #'tree-sitter-live--before-change nil t)
  (add-hook 'after-change-functions #'tree-sitter-live--after-change nil t)
  (when (null tree-sitter-live--idle-timer)
    (setq tree-sitter-live--idle-timer
          (run-with-idle-timer tree-sitter-live-idle-time
                               t #'tree-sitter-live--idle-update)))
  nil)

(provide 'tree-sitter-live)
;;; tree-sitter-defs.el ends here

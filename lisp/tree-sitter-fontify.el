;;; tree-sitter-fontify.el --- Fontify buffers using tree-sitter  -*- lexical-binding: t; -*-

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

;; Live fontification of buffers using tree-sitter. This mode builds
;; on the live-parsing provided by `tree-sitter-live-mode' to set
;; faces inside buffers. It overrides settings of several
;; `font-lock-mode' functions to hook insert itself into the
;; fontification system used in existing major modes. A globalized
;; minor mode is provided to enable fontification in buffers that have
;; a configuration in `tree-sitter-live-auto-alist'.

;;; Code:
(require 'tree-sitter)
(require 'tree-sitter-live)


;; Internal variables


;; Internal buffer-local variables
(defvar-local tree-sitter-fontify--orig-fontify-region nil)


;; Custom definitions
(defgroup tree-sitter-fontify nil
  "Options controlling fontification of buffers with tree-sitter.")


;; Internal functions
(defun tree-sitter-fontify--region (beg end &optional verbose))

(defun tree-sitter-fontify--enable ()
  ;; Save original font-lock functions
  (setq tree-sitter-fontify--orig-fontify-region font-lock-fontify-region-function)
  (setq font-lock-fontify-region-function #'tree-sitter-fontify--region))

(defun tree-sitter-fontify--disable ()
  ;; Restore original font-lock functions
  (setq font-lock-fontify-region-function tree-sitter-fontify--orig-fontify-region))


;; Other functions
(defun tree-sitter-fontify-mode-turn-on ()
  "Maybe enable `tree-sitter-fontify-mode' for a buffer.
Enable the mode when `global-tree-sitter-live-mode' would be
activated for it. Whether or not this global minor mode is
enabled."
  (cond
   (tree-sitter-live-mode
    ;; Already ready to go
    (tree-sitter-fontify-mode 1))
   (global-tree-sitter-live-mode
    ;; We can just wait for the global mode to check for us
    (add-hook 'tree-sitter-live-mode-hook #'tree-sitter-fontify-mode-turn-on 0 t))
   (t
    ;; Try to manually enable the live mode
    (progn
      (add-hook 'tree-sitter-live-mode-hook #'tree-sitter-fontify-mode-turn-on 0 t)
      (tree-sitter-live-mode-turn-on)))))


;; Minor modes

;;;###autoload
(define-minor-mode tree-sitter-fontify-mode
  ""
  :lighter ""
  :group 'tree-sitter-fontify
  (if tree-sitter-fontify-mode
      ;; Enabling the mode
      (progn
        (unless tree-sitter-live-mode
          (tree-sitter-live-mode 1))
        (if tree-sitter-live-mode
            (tree-sitter-fontify--enable)
          (error "No tree-sitter-live definition for this buffer")))
    ;; Disabling the mode
    (tree-sitter-fontify--disable)))

;;;###autoload
(define-globalized-minor-mode global-tree-sitter-fontify-mode
  tree-sitter-fontify-mode tree-sitter-fontify-mode-turn-on
  :group 'tree-sitter-fontify)

(provide 'tree-sitter-fontify)
;;; tree-sitter-fontify.el ends here

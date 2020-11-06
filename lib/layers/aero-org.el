;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(use-package org
	:defer t
	:straight org-plus-contrib
	:commands org-mode
	:mode ("\\.org\\'" . org-mode)

	:config
	(setq org-src-preserve-indentation t
				org-footnote-auto-adjust t
				org-footnote-section nil
				org-startup-with-inline-images t
				org-startup-indented t)

	;; rescale images to 400px if no with attribute is set (see
	;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
	(setq org-image-actual-width '(400))

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  (use-package toc-org :straight t)

  (aero-mode-leader-def
    :keymaps 'org-mode-map
    "h" 'org-hide-block-all)
  )

(defvar aero/org-eval-safe-list
  '(expand-file-name "~/doc/thornlog/")
  "Directories which will have their contents evaluated without prompting.")

(defun aero/safe-org-file-p (file)
  "Determine if given filename is in a safe org path.
Safe org paths are determined by `aero/org-eval-safe-list'."
  (let ((file-path (file-name-directory file)))
    (seq-some (lambda (x) (string= x file-path)) aero/org-eval-safe-list)))

(defun aero/org-eval-startblock ()
  "Evaluate the content of a code-block named 'aero/startblock' in the current
  org-document, if present.

  Emacs would usually prompt for permission as a safety precaution, but if the
  buffer is associated with a filename matching any of the patterns inside the
  list aero/org-eval-safe-list we just allow it. "
  (aero/org-eval-named-block "aero/startblock"))

(defun aero/org-eval-saveblock ()
  "Evaluate the content of a code-block named 'aero/saveblock' in the current
  org-document, if present.

  Emacs would usually prompt for permission as a safety precaution, but if the
  buffer is associated with a filename matching any of the patterns inside the
  list aero/org-eval-safe-list we just allow it."
  (aero/org-eval-named-block "aero/saveblock"))

(defun aero/org-eval-named-block (name)
  "Execute the named block, if it exists, from within the current file."
  (save-excursion
    (org-save-outline-visibility t
      (when (and (aero/safe-org-file-p (buffer-file-name))
                 (member name (org-babel-src-block-names)))
        (progn
          (setq-local org-confirm-babel-evaluate nil)
          (org-babel-goto-named-src-block name)
          (org-babel-execute-src-block))))))
(add-hook 'org-mode-hook 'aero/org-eval-startblock)

;; evaluation the save-block on save
(defun aero/org-mode-before-save-hook-eval ()
  (when (eq major-mode 'org-mode)
    (aero/org-eval-saveblock)))
(add-hook 'before-save-hook #'aero/org-mode-before-save-hook-eval)

(provide 'aero-org)

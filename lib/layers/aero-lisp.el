;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(setq inferior-lisp-program "sbcl")

(use-package hy-mode
  :ensure t
  :mode ("\\.hy\\'" . hy-mode))

;; TODO common lisp
;; TODO include Lakefile as common lisp

(defun indent-defun ()
  "Indent current defun"
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(provide 'aero-lisp)

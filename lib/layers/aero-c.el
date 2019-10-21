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

(use-package cc-mode
  :ensure t
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . cpp-mode)
         ("\\.hpp\\'" . cpp-mode))
  :preface
  (defun aero/c-mode-common-hook ()
    "Hook to run in all C modes"
    (set (make-local-variable 'parens-require-spaces) nil))
  :hook (c-mode-common . aero/c-mode-common-hook)
  :config
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang))

(provide 'aero-c)

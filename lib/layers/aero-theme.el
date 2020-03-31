;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
;;
;;; Commentary:
;;
;;; Code:

;; in etc/themes/
(load-theme 'aero-dark t)

(require 'aero-modeline)
(aero/modeline-mode 1)


;;; additional tweaks and packages

;; (when (boundp 'global-prettify-symbols-mode)
;;   (add-hook
;;    'prog-mode-hook
;;    (lambda ()
;;      (setq prettify-symbols-alist
;;            (append prettify-symbols-alist
;;                    '(
;;                      ;; add all greek
;;                      ("lambda" . ?λ)

;;                      ;; mathematics
;;                      ("and" . ?∧)
;;                      ("&&" . ?∧)
;;                      ("or" . ?∨)
;;                      ("||" . ?∨)

;;                      ;; relational algebra
;;                      ("in" . ?∈)
;;                      ("not in" . ?∉)

;;                      ;; arrows and similar
;;                      ("<=" . ?≤)
;;                      (">=" . ?≥)
;;                      ("=>" . ?⇒)
;;                      ("->" . ?→)
;;                      ("!=" . ?≠)
;;                      ("===" . ?≡)
;;                      ("!==" . ?≢)
;;                      ("<<" . ?≪)
;;                      (">>" . ?≫))))))
;;   (global-prettify-symbols-mode t))

(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-fill-column-indicator-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package formfeeder
  :load-path "lib/packages/formfeeder/"
  :hook (text-mode . formfeeder-mode)
  :config
  (setq formfeeder-line-width 80)
  (declare-function global-formfeeder-mode "formfeeder")
  (global-formfeeder-mode 1))

(use-package todo-light
  :load-path "lib/packages/todo-light/"
  :hook ((prog-mode text-mode) . todo-light-mode))

(use-package fireplace :straight t
  :commands fireplace)
(provide 'aero-theme)

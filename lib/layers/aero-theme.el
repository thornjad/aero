;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
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

(require 'subr-x)
(require 'use-package)

;;; Code:

(use-package aero-modeline
  :config
  (aero-modeline-mode 1))

(use-package autothemer :straight t)
(load-theme 'aero-dark t)


;;; headerline

(defgroup aero/headerline nil
  "A simple header-line configuration."
  :group 'header-line)

(defvar aero/headerline--current-window)

(defface aero/headerline-which-function
  '((t (:inherit (which-func) :weight bold)))
  "Face for git branch in header."
  :group 'aero/headerline)

(defsubst aero/headerline-is-active ()
  "Return t if the current window is active, nil if not."
  (eq (selected-window) aero/headerline--current-window))

(defvar-local aero/headerline--current-window (frame-selected-window))
(defun aero/headerline--update-selected-window (&rest _)
  "Update the `aero/headerline--current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq aero/headerline--current-window win)))))

(defun aero/headerline-segment-which-function ()
  "Display the current function, according to `which-function-mode'."
  (when (require 'which-func nil 'noerror)
    (let ((fun (which-function)))
      (when fun
        (concat
         " –– "
         (propertize (which-function) 'face 'aero/headerline-which-function)
         " ")))))

;; Store the original
(defvar aero/headerline--default-header-line header-line-format)

(define-minor-mode aero/headerline-mode
  "Toggle aero/headerline on or off."
  :group 'aero/headerline
  :global t
  :lighter nil
  (progn
    (add-hook 'window-configuration-change-hook #'aero/headerline--update-selected-window)
    (add-hook 'focus-in-hook #'aero/headerline--update-selected-window)
    (advice-add #'handle-switch-frame :after #'aero/headerline--update-selected-window)
    (advice-add #'select-window :after #'aero/headerline--update-selected-window)

    (setq-default header-line-format
                  '((:eval
                     (aero/info-line-format
                      ;; Left
                      (format-mode-line
                       '((:eval (aero/headerline-segment-which-function))))

                      ;; Right
                      ""))))))

(aero/headerline-mode)

;;; additional tweaks and packages

(when (boundp 'global-prettify-symbols-mode)
  (add-hook
   'prog-mode-hook
   (lambda ()
     (setq prettify-symbols-alist
           (append prettify-symbols-alist
                   '(
                     ;; add all greek
                     ;; ("Alpha" . ?Α)
                     ;; ("Beta" . ?Β)
                     ;; ("Gamma" . ?Γ)
                     ;; ("Delta" . ?Δ)
                     ;; ("Epsilon" . ?Ε)
                     ;; ("Zeta" . ?Ζ)
                     ;; ("Eta" . ?Η)
                     ;; ("Theta" . ?Θ)
                     ;; ("Iota" . ?Ι)
                     ;; ("Kappa" . ?Κ)
                     ;; ("Lambda" . ?Λ)
                     ;; ("Mu" . ?Μ)
                     ;; ("Nu" . ?Ν)
                     ;; ("Xi" . ?Ξ)
                     ;; ("Omicron" . ?Ο)
                     ;; ("Pi" . ?Π)
                     ;; ("Rho" . ?Ρ)
                     ;; ("Sigma" . ?Σ)
                     ;; ("Tau" . ?Τ)
                     ;; ("Upsilon" . ?Υ)
                     ;; ("Phi" . ?Φ)
                     ;; ("Chi" . ?Χ)
                     ;; ("Psi" . ?Ψ)
                     ;; ("Omega" . ?Ω)
                     ;; ("alpha" . ?α)
                     ;; ("beta" . ?β)
                     ;; ("gamma" . ?γ)
                     ;; ("delta" . ?δ)
                     ;; ("epsilon" . ?ε)
                     ;; ("zeta" . ?ζ)
                     ;; ("eta" . ?η)
                     ;; ("theta" . ?θ)
                     ;; ("iota" . ?ι)
                     ;; ("kappa" . ?κ)
                     ("lambda" . ?λ)
                     ;; ("mu" . ?μ)
                     ;; ("nu" . ?ν)
                     ;; ("xi" . ?ξ)
                     ;; ("omicron" . ?ο)
                     ;; ("pi" . ?π)
                     ;; ("rho" . ?ρ)
                     ;; ("sigma" . ?σ)
                     ;; ("sigma-final" . ?ς)
                     ;; ("tau" . ?τ)
                     ;; ("upsilon" . ?υ)
                     ;; ("phi" . ?φ)
                     ;; ("chi" . ?χ)
                     ;; ("psi" . ?ψ)
                     ;; ("omega" . ?ω)

                     ;; a couple hebrew/aramaic
                     ;; ("Alef" . ?א)
                     ;; ("Bet" . ?ב)
                     ;; ("alef" . ?א)
                     ;; ("bet" . ?ב)

                     ;; mathematics
                     ("and" . ?∧)
                     ("&&" . ?∧)
                     ("or" . ?∨)
                     ("||" . ?∨)
                     ;; ("nor" . ?⊽)
                     ;; ("xor" . ?⊻)
                     ;; ("nand" . ?⊼)

                     ;; relational algebra
                     ;; ("select" . ?σ)
                     ;; ("select distinct" . ?Π)
                     ("in" . ?∈)
                     ("not in" . ?∉)
                     ;; technically ⋈ is a natural join, while a bare join in
                     ;; SQL is an equi-join
                     ;; ("join" . ?⋈)
                     ;; ("union" . ?∪)
                     ;; ("intersect" . ?∩)
                     ;; ("foreach" . ?∀)
                     ;; ("!" . ?¬)
                     ;; ("!!" . ?⫬)

                     ;; arrows and similar
                     ("<=" . ?≤)
                     (">=" . ?≥)
                     ("=>" . ?⇒)
                     ;; ("<-" . ?∈)
                     ("->" . ?→)
                     ("!=" . ?≠)
                     ("===" . ?≡)
                     ("!==" . ?≢)
                     ("<<" . ?≪)
                     (">>" . ?≫)

                     ;; prog
                     ;; ("nil" . ?∅)
                     ;; ("null" . ?∅)
                     ;; ("blank" . ?␢)
                     ;; ("fn" . ?ƒ)
                     ;; ("defun" . ?ƒ)
                     ;; ("proc" . ?ƒ)
                     ;; ("function" . ?ƒ)
                     ;; ("Infinity" . ?∞)
                     ;; ("infinity" . ?∞)
                     ;; ("sum" . ?∑)
                     ;; ("ceil" . ?⎡)
                     ;; ("floor" . ?⎣)
                     )))))
  (global-prettify-symbols-mode t))

(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

(use-package formfeeder
  :load-path "lib/packages/formfeeder/"
  :hook (text-mode . formfeeder-mode)
  :config
  (declare-function global-formfeeder-mode "formfeeder")
  (global-formfeeder-mode 1))

(use-package todo-light
  :load-path "lib/packages/todo-light/"
  :hook ((prog-mode text-mode) . todo-light-mode))

(provide 'aero-theme)

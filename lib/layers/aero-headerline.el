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
    (declare-function which-function "which-func")
    (which-function-mode 1)
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

(provide 'aero-headerline)

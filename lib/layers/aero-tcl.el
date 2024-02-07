;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2022, 2024 Jade Michael Thornton
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

(require 'aero-prelude)

(package! tcl :builtin
  :mode ("\\(\\.tcl\\|\\.test\\)\\'" . tcl-mode)
  :init
  ;; make inferior-tcl use tclsh (default is wish in GNU Emacs, no effect in Aero Emacs)
  (setq tcl-application "tclsh")
  :config
  (add-to-list 'tcl-type-alist '("namespace" "eval" tcl-expr tcl-commands)))

(package! rivet-mode :auto)

(defun mc-string-at-point (&optional key)
  "Surround the current string at point with a call to `mc'.

User is prompted to provide a MC key. If user enters nothing, a dummy is used
instead. After this command, point is moved to the end of the `mc' call for the
insertion of arguments, if applicable.

This function will not do anything unless tcl-mode is the current major mode."
  (interactive "sMC key: ")
  (if (string= major-mode "tcl-mode")
      (progn
        (when (or (not key) (string= key ""))
          (setq key "TODO_ADD_KEY"))

        (let ((open (format "[mc %s " key))
              (close "]"))
          (if (and transient-mark-mode mark-active)
              (progn
                (save-excursion
                  (setf (point) (region-end))
                  (insert close))
                (setf (point) (region-beginning))
                (insert open))
            (skip-chars-forward " \t")
            (insert open)
            (save-excursion
              (forward-sexp 1)
              (insert close))
            (forward-sexp 1))))
    (message "Must be in TCL mode!")))

(with-eval-after-load 'general
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "sm" 'mc-string-at-point))

(provide 'aero-tcl)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2021 Jade Michael Thornton
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

(use-package tcl
  ;; :straight (tcl :host gitlab :repo "thornjad/tcl-mode")
  :mode ("\\.\\(tcl\\|test\\)\\'" . tcl-mode)
  :init
	;; make inferior-tcl use tclsh (default is wish)
	(setq tcl-application "tclsh")
  :config
  (add-to-list 'tcl-type-alist '("namespace" "eval" tcl-expr tcl-commands))
	(add-hook 'tcl-mode-hook (lambda () (setq-local indent-tabs-mode t))))

(use-package testbackend
  :disabled t ;; TODO need to get this working properly
  :load-path "lib/packages/testbackend"
  :init
  (aero-mode-leader-def
   :keymaps 'tcl-mode-map
   "t" '(:ignore t :which-key "testbackend")
   "tt" '(testbackend/run-tests-dwim-focus :wk "Run tests and focus")
   "tT" '(testbackend/run-tests-dwim :wk "Run tests")
   "tr" '(testbackend/run-tests-focus :wk "Prompt for run and focus")
   "tR" '(testbackend/run-tests :wk "Prompt for run")
   "tR" '(testbackend/re-run-tests :wk "Re-run tests")
   "tW" '(testbackend/run-tests-on-write :wk "Activate run tests on write")
   "tS" '(testbackend/stop-run-tests-on-write :wk "Stop run tests on write")))

(use-package rivet-mode ; local
  :mode "\\.rvt\\'")

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
                   (goto-char (region-end))
                   (insert close))
                 (goto-char (region-beginning))
                 (insert open))
             (skip-chars-forward " \t")
             (insert open)
             (save-excursion
               (forward-sexp 1)
               (insert close))
             (forward-sexp 1))))
     (message "Must be in TCL mode!")))

(general-define-key
 :states 'normal
 :prefix "SPC"
  "sm" 'mc-string-at-point)

(provide 'aero-tcl)

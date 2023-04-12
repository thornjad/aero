;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022 Jade Michael Thornton
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
;; This file is not part of GNU Emacs

;;; Commentary:
;;
;; Based on lsp-headerline (built-in to lsp-mode), `aero-eglot-headerline-mode' provides a
;; no-frills, breadcrumb headerline for Eglot.
;;
;; For the time being, this is experimental and a part of Aero Emacs. Assuming this works out
;; nicely, this will be moved to its own repository.
;;
;;; Code:

(defvar-local aero-eglot-headerline--string nil
  "Holds the current breadcrumb string on headerline.")

(defvar aero-eglot-headerline--on-idle-timer nil)
(defcustom aero-eglot-headerline-on-idle-hook nil
  "Hooks to run after `idle-update-delay' when in Eglot."
  :type 'hook
  :group 'aero)

(defun aero-eglot-headerline--idle-reschedule ()
  "Reschedule idle timer in BUFFER."
  (when aero-eglot-headerline--on-idle-timer
    (cancel aero-eglot-headerline--on-idle-timer))
  (setq aero-eglot-headerline--on-idle-timer (run-with-idle-timer
                                              idle-update-delay
                                              nil
                                              #'aero-eglot-headerline--on-idle
                                              (current-buffer))))

(defun aero-eglot-headerline--on-idle (buffer)
  "Start command loop."
  (when (and (buffer-live-p buffer)
             (equal buffer (current-buffer))
             (and (fboundp #'eglot-managed-p) (eglot-managed-p)))
    (run-hooks 'aero-eglot-headerline-on-idle-hook)))

(defun aero-eglot-headerline--check-breadcrumb (&rest _)
  "Update the breadcrumb if needed."
  (setq aero-eglot-headerline--string (aero-eglot-headerline--build-string))
  (force-mode-line-update))

(defun aero-eglot-headerline--build-string ()
  "Builds the headerline string."
  (string-trim-right
   (let ((segment-string (aero-eglot-headerline--build-symbol-string)))
     (if (eq segment-string "")
         ""
       (concat "> " segment-string " ")))))

(defun aero-eglot-headerline--build-sumbol-string ()
  "Build the symbol segment for the breadcrumb."
  (if (eglot--server-capable :documentSymbolProvider)))

(define-minor-mode aero-eglot-headerline-mode
  "Toggle simple breadcrumb on the headerline."
  :group 'aero
  :global nil
  (cond
   (aero-eglot-headerline-mode
    (require 'eglot)

    ;; make sure header-line-format, if non-nil, is a list, so we can treat it as such.
    (unless (listp header-line-format)
      (setq header-line-format (list header-line-format)))
    (add-to-list 'header-line-format '(t (:eval aero-eglot-headerline--string)))

    (add-hook 'xref-after-jump-hook #'aero-eglot-headerline--check-breadcrumb 0 t)
    (add-hook 'aero-eglot-headerline-on-idle-hook #'aero-eglot-headerline--check-breadcrumb 0 t)
    (add-hook 'post-command-hook #'aero-eglot-headerline--idle-reschedule 0 t))

   (t
    (remove-hook 'xref-after-jump-hook #'aero-eglot-headerline--check-breadcrumb t)
    (remove-hook 'aero-eglot-headerline-on-idle-hook #'aero-eglot-headerline--check-breadcrumb t)
    (remove-hook 'post-command-hook #'aero-eglot-headerline--idle-reschedule t))))

(provide 'aero-eglot-headerline)

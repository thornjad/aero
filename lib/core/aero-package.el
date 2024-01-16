;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022-2024 Jade Michael Thornton
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
;; Aero custom packaging system, a wrapper around use-package and straight. Note that we cannot use
;; aero-lib here yet, this file is loaded earlier than the rest of core.
;;
;;; Code:

;; Use the more-cutting-edge develop branch of straight
(eval-when-compile
  (defvar straight-repository-branch)
  (defvar straight-check-for-modifications))
(setq straight-repository-branch "develop")

;; Don't allow straight to check for modifications in every repo on Emacs init, saving some startup
;; time
(setq straight-check-for-modifications nil)

;; Tell straight that let-alist is a built-in package now, so it doesn't need to be checked if we
;; (or more likely any dependency) try to pull it in.
(with-eval-after-load 'straight
  (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (setf (point) (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; use-package

;; Not certain we need to set up straight first, but something got messed up back before use-package
;; was built-in, so now it will never ever move.
(require 'use-package)

;; Only expand minimally if we're byte-compiling, and only use verbose if we're in --debug-init.
(eval-when-compile
  (defvar use-package-expand-minimally)
  (defvar use-package-compute-statistics)
  (defvar use-package-minimum-reported-time)
  (defvar use-package-verbose))
(setq use-package-expand-minimally byte-compile-current-file
      use-package-compute-statistics nil ; t then `use-package-report' to find packages not used
      use-package-minimum-reported-time 0.1
      use-package-verbose init-file-debug)


;; Aero package macro

(defmacro package! (package recipe &rest body)
  "Get PACKAGE using RECIPE, then evaluate PACKAGE & BODY with `use-package'.

Example:

    (package! foo (:host gitlab :repo \"thornjad/foo\" :branch \"main\")
     :commands (foo-bar foo-spam))

If the RECIPE is :builtin or :local, do not search [M]ELPA, only pass BODY to `use-package'. While
there is no functional difference between these two keywords, :builtin should be used for packages
within Emacs while :local should be used for user packages which exist locally. :local packages may
require a :load-path for `use-package' to load properly.

If the BODY contains the keyword :disabled, the package is completely ignored, with an expansion
indicating the package has been disabled.

DEPRECATED: If the RECIPE is :auto, use the recipe provided by [M]ELPA. This is deprecated in favor of providing an explicit recipe. A recipe allows greater control over packages while also providing an easier path to cutting-edge updates.

Usage of this macro allows simplified refactoring when changing packaging systems, as Aero is wont
to do every few years."
  (declare (indent defun)) ; indent like use-package

  (when (memq :auto body)
    (display-warning
     'aero
     (format "Package %s uses :auto, which is deprecated. Specify recipe instead" package)
     :warning))

  (cond
   ((memq :disabled body)
    (format "%s :disabled by Aero package!" package))

   ((or (equal recipe :builtin) (equal recipe :local))
    `(use-package ,package ,@body))

   ;; Use straight
   (t
    `(use-package ,package :straight ,(or (equal recipe :auto) recipe) ,@body))

   ;; Disabled until elpaca matures a little more, it's still in active development, has too many
   ;; bugs for daily use as of this comment.
   (nil
    `(elpaca-use-package
      ,(cond
        ((equal recipe :auto)
         package)
        ((keywordp (car recipe))
         ;; Elpaca prefers recipe to start with identifier, but it's redundant in usage, so
         ;; auto-insert it here if recipe doesn't have it.
         (cons package recipe))
        (t
         recipe))
      ,@body))))

(provide 'aero-package)

;;; aero-package.el ends here

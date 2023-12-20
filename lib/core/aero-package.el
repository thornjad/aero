;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2022-2023 Jade Michael Thornton
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
;; Aero custom packaging system, a wrapper around use-package and straight, with package repository
;; priority given to ThELPA.

(require 'straight)

;;; Code:

(defmacro package! (package recipe &rest body)
  "Get PACKAGE using RECIPE, then evaluate PACKAGE & BODY with `use-package'.

Example:

    (package! foo :auto :commands (foo-bar foo-spam))

If the RECIPE is :auto, use the recipe provided by [M]ELPA.

If the RECIPE is :builtin or :local, do not search [M]ELPA, only pass BODY to `use-package'. While
there is no functional difference between these two keywords, :builtin should be used for packages
within Emacs while :local should be used for user packages which exist locally. :local packages may
require a :load-path for `use-package' to load properly.

If the BODY contains the keyword :disabled, the package is completely ignored, with an expansion
indicating the package has been disabled.

Usage of this macro allows simplified refactoring when changing packaging systems, as Aero is wont
to do every few years."
  (declare (indent defun)) ; indent like use-package
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

(defun straight-recipes-thelpa-retrieve (package)
  "Look up a PACKAGE recipe in ThELPA.

PACKAGE should be a symbol. If the package has a recipe listed in
ThELPA that uses one of the Git fetchers, return it; otherwise
return nil."
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally (expand-file-name (symbol-name package) "recipes/"))
          (let ((thelpa-recipe (read (current-buffer)))
                (plist nil))
            (cl-destructuring-bind (name . thelpa-plist) thelpa-recipe
              (straight--put plist :type 'git)
              (straight--put plist :flavor 'thelpa)
              (when-let ((files (plist-get thelpa-plist :files)))
                ;; We must include a *-pkg.el entry in the recipe
                ;; because that file always needs to be linked over,
                ;; if it is present, but the `:files' directive might
                ;; not include it (and doesn't need to, because THELPA
                ;; always re-creates a *-pkg.el file regardless). See
                ;; https://github.com/radian-software/straight.el/issues/336.
                (straight--put plist :files
                               (append files (list (format "%S-pkg.el" package)))))

              (when-let ((branch (plist-get thelpa-plist :branch)))
                (straight--put plist :branch branch))

              (pcase (plist-get thelpa-plist :fetcher)
                ('git (straight--put plist :repo (plist-get thelpa-plist :url)))
                ((or 'github 'gitlab 'codeberg 'sourcehut)
                 (straight--put plist :host (plist-get thelpa-plist :fetcher))
                 (straight--put plist :repo (plist-get thelpa-plist :repo)))
                ;; This error is caught by `condition-case', no need
                ;; for a message.
                (_ (error "")))
              (cons name plist))))
      (error nil))))

(defun straight-recipes-thelpa-list ()
  "Return a list of recipes available in THELPA, as a list of strings."
  (straight--directory-files "recipes" "^[^.]"))

(defun straight-recipes-thelpa-version ()
  "Return the current version of the THELPA retriever."
  1)

(straight-use-recipes '(thelpa :type git :host github
                               :repo "thornjad/thelpa"
                               :build nil))

(setq straight-recipe-repositories
      '(thelpa org-elpa melpa gnu-elpa-mirror nongnu-elpa el-get emacsmirror-mirror))

(provide 'aero-package)

;;; aero-package.el ends here

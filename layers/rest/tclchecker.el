;;; tclchecker.el --- A flycheck checker for Tcl using tclchecker

;; Copyright (C) 2019 Jade Michael Thornton
;; Copyright (C) 2014-2016 Niels Widger

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;;; Setup

;; Add the following to your init file:
;;
;;    (with-eval-after-load 'flycheck
;;      (tclchecker-setup))

;;; Code:

(require 'flycheck)

(flycheck-def-option-var tclchecker-tclchecker-use-packages nil tcl-tclchecker
  "A list of specific Tcl packages to check with `-use'.

The value of this variable is a list of strings, where each
string is a package name with an optional version number attached such as `Tcl' or `Tcl8.6'."
  :type '(repeat (string :tag "Package name (optionally with version)"))
  :safe #'flycheck-string-list-p
  :package-version '(flycheck . "0.17"))

(flycheck-define-checker tcl-tclchecker
  "A Tcl checker using ActiveState's tclchecker."
  :command ("tclchecker" "-quiet" "-W2" (option-list "-use" tclchecker-tclchecker-use-packages) source)
  :error-patterns
  ((warning line-start (file-name) ":" line " (warn" (one-or-more (any alpha)) ") " (message) line-end)
   (error line-start (file-name) ":" line " (" (one-or-more (any alpha)) ") " (message) line-end))
  :modes tcl-mode)

;;;###autoload
(defun tclchecker-setup ()
  "Setup Flycheck Tcl.
Add `tcl-tclchecker' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'tcl-tclchecker))

(provide 'tclchecker)

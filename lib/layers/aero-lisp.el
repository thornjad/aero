;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton
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

(require 'aero-prelude)

(package! common-lisp-mode :builtin
  :mode "\\(Lakefile|\\.\\(cl|lisp\\)\\)\\'")

(package! slime :auto
  :commands slime
  :init
  (setq-default
   inferior-lisp-program "ecl"
   slime-contribs '(slime-fancy))
  ;; Load SBCL faster by using preset socket and POSIX shit.
  ;; NOTE: this requires some set-up beforehand in the SBCL REPL:
  ;;   * (mapc 'require '(sb-bsd-sockets sb-posix sb-introspect sb-cltl2 asdf))
  ;;   * (save-lisp-and-die "sbcl.core-for-slime")
  (defvar slime-lisp-implementations)
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "sbcl.core-for-slime"))
          (ecl ("ecl")))))

(defun indent-defun ()
  "Indent current defun"
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

;; redefined to turn this:
;; (:foo bar
;;       :spam ham)
;; into this:
;; (:foo bar
;;  :spam ham)
(eval-after-load "lisp-mode"
  '(defun lisp-indent-function (indent-point state)
     "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine if the arguments of
a Lisp function call should be indented specially. INDENT-POINT is the position
at which the line being indented begins. Point is located at the point to indent
under (for default indentation); STATE is the `parse-partial-sexp' state for
that position. If the current line is in a call to a Lisp function that has a
non-nil property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent. The property value can be:

* `defun', meaning indent `defun'-style \(this is also the case if there is no
  property and the function has a name that begins with \"def\", and three or
  more arguments);

* an integer N, meaning indent the first N arguments specially (like ordinary
  function arguments), and then indent any further arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments that it
  itself received.

This function returns either the indentation to use, or nil if the Lisp function
does not specify a special indentation."
     (let ((normal-indent (current-column))
           (orig-point (point)))
       (setf (point) (1+ (elt state 1)))
       (defvar calculate-lisp-indent-last-sexp)
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (cond
        ;; car of form doesn't seem to be a symbol, or is a keyword
        ((and (elt state 2)
              (or (not (looking-at "\\sw\\|\\s_"))
                  (looking-at ":")))
         (if (not (> (save-excursion (forward-line 1) (point))
                     calculate-lisp-indent-last-sexp))
             (progn (setf (point) calculate-lisp-indent-last-sexp)
                    (beginning-of-line)
                    (parse-partial-sexp (point)
                                        calculate-lisp-indent-last-sexp 0 t)))
         ;; Indent under the list or under the first sexp on the same line as
         ;; calculate-lisp-indent-last-sexp. Note that first thing on that line
         ;; has to be complete sexp since we are inside the innermost containing
         ;; sexp.
         (backward-prefix-chars)
         (current-column))
        ((and (save-excursion
                (setf (point) indent-point)
                (skip-syntax-forward " ")
                (not (looking-at ":")))
              (save-excursion
                (setf (point) orig-point)
                (looking-at ":")))
         (save-excursion
           (setf (point) (+ 2 (elt state 1)))
           (current-column)))
        (t
         (let ((function (buffer-substring (point)
                                           (progn (forward-sexp 1) (point))))
               method)
           (setq method (or (function-get (intern-soft function)
                                          'lisp-indent-function)
                            (get (intern-soft function) 'lisp-indent-hook)))
           (cond ((or (eq method 'defun)
                      (and (null method)
                           (> (length function) 3)
                           (string-match "\\`def" function)))
                  (lisp-indent-defform state indent-point))
                 ((integerp method)
                  (lisp-indent-specform method state
                                        indent-point normal-indent))
                 (method
                  (funcall method indent-point state)))))))))

(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'common-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))


;; Clojure

(package! clojure-mode :auto :mode "\\.\\(cljs?\\|cljs.*\\|edn\\|boot\\)\\'")
(package! cider :auto
  :hook ((clojure-mode . cider-mode)
         (cider-mode . eldoc-mode))
  :after (clojure-mode general)
  :commands (cider
             cider-jack-in)
  :custom
  (cider-show-error-buffer t)
  (cider-repl-history-file (expand-file-name "cider-history" aero-cache-dir))
  :config
  (aero-mode-leader-def
    :keymaps '(clojure-mode-map cider-mode-map)
    "c" '(:ignore t :wk "cider")
    "c RET" 'cider-run
    "cj" 'cider-jack-in
    "c'" 'cider-switch-to-repl-buffer
    "c," 'cider-pop-back
    "cQ" 'cider-quit
    "cl" '(:ignore t :wk "load")
    "clb" 'cider-load-buffer
    "clf" 'cider-load-file
    "cld" '(cider-load-all-files :wk "load directory")
    "cR" 'cider-ns-refresh
    "ch" '(:ignore t :wk "help")
    "chd" 'cider-doc
    "chj" 'cider-javadoc
    "chc" 'cider-clojuredocs
    "chC" 'cider-clojuredocs-web
    "cha" 'cider-apropos
    "chD" 'cider-apropos-documentation
    "ct" '(:ignore t :wk "test")
    "ctt" 'cider-test-run-test
    "ctr" 'cider-test-rerun-test
    "ctn" 'cider-test-run-ns-tests
    "ctp" 'cider-test-run-project-tests
    "ctf" 'cider-test-rerun-failed-tests
    "ctp" 'cider-test-show-report
    "cb" 'cider-load-buffer-and-switch-to-repl-buffer
    "cd" 'cider-eval-defun-at-point
    "cs" 'cider-eval-sexp-at-point
    "cr" 'cider-eval-region
    "cm" '(:ignore t :wk "macro expand")
    "cmm" 'cider-macroexpand-1
    "cma" 'cider-macroexpand-all
    "cN" 'cider-eval-ns-form
    "ce" '(:ignore t :wk "echo")
    "cee" '(cider-eval-last-sexp :wk "echo last sexp")
    "cer" '(cider-eval-last-sexp-to-repl :wk "eval last sexp to repl")
    "cep" '(cider-pprint-eval-last-sexp :wk "pprint last sexp"))

  (with-eval-after-load 'lsp-mode
    (aero-mode-leader-def
      :keymaps 'clojure-mode-map
      "r" '(:ignore t :wk "refactor")
      "rt" '(:ignore t :wk "thread")
      "rtt" 'lsp-clojure-thread-first
      "rtT" 'lsp-clojure-thread-first-all
      "rtl" 'lsp-clojure-thread-last
      "rtL" 'lsp-clojure-thread-last-all
      "rL" 'lsp-clojure-add-missing-libspec
      "rC" 'lsp-clojure-cycle-coll
      "rl" '(:ignore t :wk "let")
      "rle" 'lsp-clojure-expand-let
      "rli" 'lsp-clojure-introduce-let
      "rlm" 'lsp-clojure-move-to-let
      "rU" 'lsp-clojure-unwind-all
      "rp" 'lsp-clojure-cycle-privacy
      "re" 'lsp-clojure-extract-function
      "rs" 'lsp-clojure-inline-symbol)))


;; Elisp-specific

(package! package-lint :auto
  :commands (package-lint-current-buffer))

(package! elisp-autofmt :auto
  :commands (elisp-autofmt-buffer
             elisp-autofmt-region)
  :custom
  (elisp-autofmt-cache-directory
   (expand-file-name "elisp-autofmt-cache" aero-cache-dir)))

(package! el2md (:host gitlab :repo "thornjad/el2md")
  :after (general)
  :commands (el2md-write-readme
             el2md-view-buffer
             el2md-write-file)
  :init
  (aero-mode-leader-def
    :keymaps 'emacs-lisp-mode-map
    "m" '(:ignore t :wk "el2md")
    "mr" 'el2md-write-readme
    "mv" 'el2md-view-buffer
    "mw" 'el2md-write-file))

(provide 'aero-lisp)

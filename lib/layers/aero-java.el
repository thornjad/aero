;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2020-2021 Jade Michael Thornton
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

;;; Commentary:

;;; Code:

(require 'aero-prelude)

(use-package clojure-mode :straight t)
(use-package cider :straight t
  :hook ((clojure-mode) . cider-mode)
  :after (clojure-mode general)
  :commands (cider
             cider-jack-in)
  :config
  (aero-mode-leader-def
    :keymaps '(clojure-mode-map cider-mode-map)
    "c" '(:ignore t :wk "cider")
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

(provide 'aero-java)

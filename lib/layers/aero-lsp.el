;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton
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

;; NOTE: eglot config is in aero-prog.el

(package! lsp-mode :auto :disabled t
  :after (general)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands (lsp lsp-deferred)
  :config
  (aero-leader-def
    "lB" '(lsp-headerline-breadcrumb-mode :wk "breadcrumbs")
    "la" 'lsp-execute-code-action
    "lf" '(:ignore t :wk "find")
    "lfd" 'lsp-find-definition
    "lft" 'lsp-find-type-definition
    "lfa" '(xref-find-apropos :wk "find symbols matching pattern")
    "ld" 'lsp-describe-thing-at-point
    "l TAB" '(:ignore t :wk "format")
    "l TAB TAB" 'lsp-format-buffer
    "l TAB r" 'lsp-format-region
    "lr" '(:ignore t :wk "refactor")
    "lrr" 'lsp-rename
    "lro" 'lsp-organize-imports
    "lS" '(:ignore t :wk "server")
    "lSr" '(lsp :wk "server restart")
    "lSd" 'lsp-describe-session)

  (setq company-minimum-prefix-length 1
        lsp-log-max 500 ; default is 1000, just save a little space
        lsp-idle-delay 0.5 ; default is 0.5
        lsp-lens-enable t
        lsp-enable-suggest-server-download nil ; don't try to install automatically
        lsp-completion-provider :capf
        lsp-keep-workspace-alive nil
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-breadcrumb-icons-enable nil
        lsp-headerline-arrow "»"
        lsp-enable-file-watchers nil ; burns through max files
        lsp-enable-on-type-formatting t
        lsp-eldoc-enable-hover t ; show documentation in minibuffer on hover

        ;; unused by aero modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil)

  ;; For some reason lsp-graphql activates by default on everything js-related. This is dumb, we
  ;; only want it in graphql files, so override the check function
  (with-eval-after-load 'lsp-graphql
    (defun lsp-graphql-activate-p (filename &optional _)
      "Check if the GraphQL language server should be enabled based on FILENAME."
      (string-match-p (rx (one-or-more anything) "." (or  "graphql" "gql") eos) filename))))

(use-package lsp-treemacs :straight t
  :after (general lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode +1)

  (aero-leader-def
    "le" 'lsp-treemacs-errors-list
    "ls" 'lsp-treemacs-symbols
    "lt" 'lsp-treemacs-type-hierarchy
    "lf" '(:ignore t :wk "find")
    "lfr" '(lsp-treemacs-references :wk "find references")
    "lfi" '(lsp-treemacs-implementations :wk "find implementations")))

(package! lsp-ui :auto
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-mode . lsp-ui-sideline-toggle-symbols-info))
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'top
   lsp-ui-doc-include-signature nil
   lsp-ui-doc-delay 1
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-webkit nil  ; appears broken, https://github.com/emacs-lsp/lsp-ui/issues/349
   lsp-ui-doc-show-with-cursor t
   lsp-ui-imenu-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-sideline-show-diagnostics nil)

  (aero-leader-def
    "li" 'lsp-ui-imenu
    "lp" '(:ignore t :wk "peek")
    "lpd" '(lsp-ui-peek-find-definitions :wk "peek definitions")
    "lpr" '(lsp-ui-peek-find-references :wk "peek references")
    "lps" '(lsp-ui-peek-find-workspace-symbol :wk "peek symbol")
    "lpi" '(lsp-ui-peek-find-implementation :wk "peek implementations")
    "lff" '(lsp-ui-doc-focus-frame :wk "focus frame"))

  (evil-define-key 'normal 'lsp-ui-doc-frame-mode
    [?q] #'lsp-ui-doc-unfocus-frame))

(package! lsp-ivy :auto :defer t
  :config
  (aero-leader-def
    "lf" '(:ignore t :wk "find")
    "lfs" '(lsp-ivy-workspace-symbol :wk "find symbols")
    "lfg" '(lsp-ivy-global-workspace-symbol :wk "global find symbols")))

(provide 'aero-lsp)

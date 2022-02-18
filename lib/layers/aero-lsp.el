;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2021 Jade Michael Thornton
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

(use-package lsp-mode :straight t
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

  (defun load-lsp-format-buffer-hook ()
    "Adds a buffer-local hook to format the buffer with LSP."
    (when (fboundp #'lsp-format-buffer)
      (add-hook 'after-save-hook #'lsp-format-buffer nil t)))
  (add-hook 'lsp-mode-hook #'load-lsp-format-buffer-hook)

  (setq company-minimum-prefix-length 1
        company-idle-delay 0.2 ; default is 0.2
        lsp-idle-delay 0.2 ; default is 0.5
        lsp-lens-enable t
        lsp-completion-provider :capf
        lsp-keep-workspace-alive nil
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-arrow "»"
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers nil ; burns through max files
        lsp-enable-on-type-formatting t

        ;; unused by aero modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil))

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

(use-package lsp-java
  :hook ((java-mode . lsp)))

(use-package lsp-ui :straight t
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-mode . lsp-ui-sideline-toggle-symbols-info))
  :config
  (setq
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'top
   lsp-ui-doc-delay 1
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-use-webkit nil  ; appears broken, https://github.com/emacs-lsp/lsp-ui/issues/349
   lsp-ui-imenu-enable nil
   lsp-ui-sideline-delay 1
   lsp-ui-sideline-show-hover t
   lsp-ui-sideline-show-symbol t
   lsp-ui-sideline-show-diagnostics t
   lsp-ui-sideline-show-code-actions t)

  (aero-leader-def
    "li" 'lsp-ui-imenu
    "lp" '(:ignore t :wk "peek")
    "lpd" '(lsp-ui-peek-find-definitions :wk "peek definitions")
    "lpr" '(lsp-ui-peek-find-references :wk "peek references")
    "lps" '(lsp-ui-peek-find-workspace-symbol :wk "peek symbol")
    "lpi" '(lsp-ui-peek-find-implementation :wk "peek implementations")))

(use-package lsp-ivy :straight t
  :config
  (aero-leader-def
    "lf" '(:ignore t :wk "find")
    "lfs" '(lsp-ivy-workspace-symbol :wk "find symbols")
    "lfg" '(lsp-ivy-global-workspace-symbol :wk "global find symbols")))

(provide 'aero-lsp)

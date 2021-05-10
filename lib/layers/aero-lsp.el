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

(use-package lsp-mode :after (general)
  :hook ((sh-mode . lsp) ;; bash-language-server
         ;; (rjsx-mode . lsp) ;; javascript-typescript-langserver
         (js-mode . lsp) ;; javascript-typescript-langserver
         (python-mode . lsp) ;; python-language-server
         (rust-mode . lsp) ;; rls
         (scss-mode . lsp) ;; vscode-css-languageserver-bin
         (java-mode . lsp) ;; eclipse JDT language server
         (nix-mode . lsp) ;; rnix-lsp
         (tuareg-mode . lsp) ;; ocaml-lsp-server
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands (lsp)
  :config

  ;; FIXME this created a dozen or so language server processes on hopnu which
  ;; used a shit-ton of resources. Disabled until it can be addressed.
  ;; (lsp-register-client
  ;;  (make-lsp-client
  ;;   :new-connection (lsp-tramp-connection "javascript-language-server.js")
  ;;   :major-modes '(rjsx-mode js-mode)
  ;;   :remote? t
  ;;   :priority -1
  ;;   :server-id 'javascript-remote))

  (aero-leader-def
    "lB" '(lsp-headerline-breadcrumb-mode :wk "breadcrumbs")
    "lf" '(:ignore t :wk "find")
    "lfd" '(lsp-find-definition :wk "find definition")
    "lfr" '(lsp-find-references :wk "find references")
    "ld" 'lsp-describe-thing-at-point
    "ls" '(:ignore t :wk "server")
    "lsr" '(lsp :wk "server restart")
    "lsd" 'lsp-describe-session)

  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0 ; default is 0.2
        lsp-completion-provider :capf
        lsp-keep-workspace-alive nil
        lsp-enable-folding nil ; not used
        lsp-enable-snippet nil ; not used
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-segments '(path-up-to-project symbols))

  (use-package lsp-java
    :hook ((java-mode . lsp)))

  (use-package lsp-ui :straight t
    :hook ((lsp-mode . lsp-ui-mode)
           (lsp-ui-mode . lsp-ui-sideline-toggle-symbols-info))
    :config
    (setq lsp-ui-doc-position 'top
          lsp-ui-doc-delay 1
          lsp-ui-doc-use-childframe t
          lsp-ui-doc-use-webkit nil  ; appears broken, https://github.com/emacs-lsp/lsp-ui/issues/349
          lsp-ui-sideline-delay 1
          lsp-ui-sideline-show-hover t
          lsp-ui-sideline-show-symbol t
          lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-code-actions t)
    (aero-leader-def
      "li" 'lsp-ui-imenu
      "lp" '(:ignore t :wk "peek")
      "lpd" '(lsp-ui-peek-find-definitions :wk "peek definitions")
      "lpr" '(lsp-ui-peek-find-references :wk "peek references")))

  (use-package lsp-ivy :straight t
    :config
    (aero-leader-def
      "lf" '(:ignore t :wk "find")
      "lfs" '(lsp-ivy-workspace-symbol :wk "find symbols")
      "lfg" '(lsp-ivy-global-workspace-symbol :wk "global find symbols"))))

(use-package dap-mode :after (general)
  :config
  (require 'dap-firefox)
  (dap-firefox-setup)
  (require 'dap-node)
  (dap-node-setup)
  (require 'dap-python)

  (aero-leader-def
    "dd" 'dap-debug))

(provide 'aero-lsp)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2023 Jade Michael Thornton
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

(package! company :auto
  ;; Standard completions library
  :after (evil)
  :hook ((prog-mode . company-mode)
         (company-mode-hook . evil-normalize-keymaps))
  :init
  (setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15
        company-tooltip-margin 2
        company-require-match nil
        company-show-numbers t
	      company-tooltip-align-annotations t
        company-dabbrev-other-buffers t ; only look in open buffers with same major mode
        company-global-modes '(not
                               erc-mode message-mode help-mode gud-mode vterm-mode))
	:config
	;; Wait until it's defined, then disable preview after point
  (setq company-frontends (delq 'company-preview-if-just-one-frontend company-frontends)))

(package! company-prescient :auto
  ;; Move commonly-used completions to the top
  :after (company)
  :hook (company-mode . company-prescient-mode)
  :custom (prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir))
  :config (prescient-persist-mode +1))

(package! company-box :auto
  ;; Better popup interface for company
  :hook (company-mode . company-box-mode))


;; LSP

(package! lsp-mode :auto
  :after (general)
  :hook ((prog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-headerline-breadcrumb-mode))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-idle-delay 0.5)
  (lsp-lens-enable t)
  (lsp-completion-provider :capf)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-headerline-arrow "»")
  (lsp-enable-file-watchers nil) ; burns through max files
  (lsp-enable-on-type-formatting t)
  (lsp-eldoc-enable-hover t) ; show documentation in minibuffer on hover
  (lsp-use-plists t) ; requires shell env LSP_USE_PLISTS=true
  (lsp-warn-no-matched-clients nil) ; don't warn when no matching client for mode

  ;; graphql is really annoying, and for some reason angular-ls take priority over ts-ls, and ts-ls
  ;; is way better so we use that
  (lsp-disabled-clients '(graphql-lsp angular-ls))

  ;; unused by aero modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)

  :config
  (aero-leader-def
    "la" 'lsp-execute-code-action
    "lr" '(:ignore t :wk "references")
    "lrf" 'lsp-find-definition
    "lrt" 'lsp-find-type-definition
    "lra" '(xref-find-apropos :wk "find symbols matching pattern")
    "lrr" 'lsp-rename
    "lro" 'lsp-organize-imports
    "ld" 'lsp-describe-thing-at-point
    "lt" '(:ignore t :wk "text")
    "ltf" 'lsp-format-buffer
    "ltr" 'lsp-format-region
    "lS" '(:ignore t :wk "server")
    "lSr" '(lsp :wk "server restart")
    "lSd" 'lsp-describe-session)

  ;; Snyk language server
  (defvar snyk-ls-token "")
  (defvar snyk-ls-initialization-options `(:integrationName "Emacs"
                                           :integrationVersion ,emacs-version
                                           :token ,snyk-ls-token
                                           :activateSnykCodeSecurity "true"
                                           :activateSnykCodeQuality "true")
    "Initialization options plist for Snyk language server.")

  (lsp-register-client
   (make-lsp-client
    :server-id 'snyk-ls
    :new-connection (lsp-stdio-connection '("snyk-ls" "-o" "md"))
    :major-modes '(python-mode
                   python-ts-mode
                   typescript-mode
                   typescript-ts-mode)
    :initialization-options (lambda () snyk-ls-initialization-options)
    :add-on? t
    :priority -2))

  (defun aero/snyk-code-test ()
    "Run Snyk Code Test on the current project."
    (interactive)
    (let ((default-directory (project-root (project-current))))
      (async-shell-command "snyk code test" "*Snyk Code Test*"))))

(package! lsp-treemacs :auto
  :after (general lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode +1)
  (aero-leader-def
    "le" 'lsp-treemacs-errors-list
    "ls" 'lsp-treemacs-symbols
    "lh" 'lsp-treemacs-type-hierarchy))

(package! lsp-ui :auto
  :after (general)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-mode . lsp-ui-sideline-toggle-symbols-info))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-delay 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit nil)  ; appears broken, https://github.com/emacs-lsp/lsp-ui/issues/349
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics nil)

  :config
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

(package! lsp-ivy :auto
  :after general
  :commands (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :init
  (aero-leader-def
    "lf" '(:ignore t :wk "find")
    "lfs" '(lsp-ivy-workspace-symbol :wk "find symbols")
    "lfg" '(lsp-ivy-global-workspace-symbol :wk "global find symbols")))

;; Used by Eglot, but we want to always have the latest
(package! jsonrpc :auto)

(package! eglot :builtin :disabled
  :hook ((python-mode
          python-ts-mode
          scss-mode
          css-mode
          clojure-mode
          java-mode
          sh-mode
          json-mode
          json-ts-mode
          js2-mode
          typescript-mode
          typescript-ts-mode
          tsx-ts-mode
          elm-mode
          nix-mode
          tuareg-mode
          rust-mode)
         . eglot-ensure)
  :commands (eglot-ensure)
  :after (general)
  :custom
  (eglot-confirm-server-initiated-edits nil) ; don't ask to edit file immediately after I told it to
  (eglot-autoshutdown t) ; shutdown server after killing last managed buffer
  (eglot-events-buffer-size 0) ; disable event logging
  (eglot-send-changes-idle-time 0.75)
  ;; lsp highlighting is ridiculously slow, we use highlight-thing instead
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :config
  ;; Re-add flymake checkers because eglot clobbers them all on server start
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-base-mode)
                (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))))

  ;; Experimental homebrew LSP headerline, without any frills
  ;; (require 'aero-eglot-headerline)

  (aero-leader-def
    "la" 'eglot-code-actions
    "lf" '(:ignore t :wk "find")
    "lfr" 'xref-find-references
    "lfd" 'eglot-find-declaration
    "lfi" 'eglot-find-implementation
    "lft" 'eglot-find-typeDefinition
    "lr" '(:ignore t :wk "refactor")
    "lrr" 'eglot-rename
    "lrf" 'eglot-format
    "lro" 'eglot-code-action-organize-imports))

;; puts eldoc in a child frame instead of the echo area
(package! eldoc-box :auto
  :after general
  :config
  ;; (setq eldoc-echo-area-use-multiline-p nil) ; stop normal eldoc from resizing
  (defun aero/eldoc-box-help-at-point ()
    (interactive)
    (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
        (call-interactively #'eldoc-box-eglot-help-at-point)
      (call-interactively #'eldoc-box-help-at-point)))
  (aero-leader-def
    "i" 'aero/eldoc-box-help-at-point
    "li" 'eldoc-box-eglot-help-at-point)
  (with-eval-after-load 'eglot
    ;; Show all of the available eldoc information when we want it. This way Flymake errors
    ;; don't just get clobbered by docstrings.
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                "Make sure Eldoc will show us all of the feedback at point."
                (setq-local eldoc-documentation-strategy
                            #'eldoc-documentation-compose)))))


;; C language

(package! cc-mode :auto
  :after flymake
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . cpp-mode)
         ("\\.hpp\\'" . cpp-mode))
  :preface
  (defun aero/c-mode-common-hook ()
    "Hook to run in all C modes"
    (set (make-local-variable 'parens-require-spaces) nil))
  :hook (c-mode-common . aero/c-mode-common-hook))


;; Markup

(package! markdown-mode :auto
  :after (general smartparens)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode))

  :custom
  ;; Fix table to teach it that quotes mean string, regardless of what the dev says
  (markdown-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (markdown-header-scaling t)
  (markdown-display-remote-images t)
  (markdown-header-scaling-values '(1.3 1.2 1.1 1.0 1.0 1.0))
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-additional-languages '("sh"))

  :init
  (add-hook 'markdown-mode-hook #'flyspell-mode)

  :config
  ;; Don't expand checkboxes
  (sp-local-pair 'gfm-mode "- [ " "]")

  (require 'aero-thornlog)
  (aero-mode-leader-def
    :keymaps 'markdown-mode-map
    "t" 'today
    "d" 'new-day))

(package! markdown-toc :auto
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(package! yaml-mode :auto :mode "\\.ya?ml\\'")

(package! org :builtin
  :commands org-mode
  :config
  (setq org-src-preserve-indentation t
	      org-footnote-auto-adjust t
	      org-footnote-section nil
	      org-startup-with-inline-images t
	      org-startup-indented t)

  ;; re-scale images to 400px if no with attribute is set (see
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
  (setq org-image-actual-width '(400))

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  ;; start with all levels collapsed
  (add-hook 'org-mode-hook #'org-hide-block-all))


;; flymake

(package! flymake :builtin
  :after (general)
  :custom
  ;; left-fringe is the default, but we're being explicit because git-gutter also uses left-fringe.
  ;; Usually this works itself out.
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-wrap-around t)
  (flymake-no-changes-timeout 0.6)

  :config
  ;; Use ruff with python
  (add-hook 'python-base-mode-hook 'flymake-mode)
  (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))

  (aero-leader-def
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-show-buffer-diagnostics))

;; makes flymake appear in popup
(package! flymake-diagnostic-at-point (:host github :repo "meqif/flymake-diagnostic-at-point")
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(package! flymake-eslint :auto
  :init
  ;; Need to add after eglot so eglot doesn't clobber
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (or (derived-mode-p 'typescript-mode) (derived-mode-p 'js-mode))
                (flymake-eslint-enable)))))

(package! flymake-mypy (:host github :repo "com4/flymake-mypy")
  :init
  ;; Need to add after eglot so eglot doesn't clobber
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'python-mode)
                (flymake-mypy-enable)))))

(package! flyspell :builtin
  :after (general)
  :hook ((prog-mode . flyspell-prog-mode)
	       (text-mode . flyspell-mode))
  :config
  (defvar aero-etc-dir)
  (setq
   flyspell-issue-message-flag nil
   ispell-personal-dictionary (expand-file-name
                               "ispell/personal_dictionary.aws"
                               aero-etc-dir)
   flyspell-sort-corrections nil)

  ;; Skip code inside org src blocks
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (aero-leader-def
    "ps" '(:ignore t :wk "spelling")
    "psP" 'flyspell-prog-mode
    "psc" 'flyspell-correct-wrapper
    "psC" 'flyspell-correct-at-point
    "psp" 'flyspell-correct-previous
    "psn" 'flyspell-correct-next
    "psw" 'flyspell-word
    "psb" 'flyspell-buffer
    "psr" 'flyspell-region))

(package! flyspell-lazy (:host github :repo "rolandwalker/flyspell-lazy")
  :hook ((flyspell-mode . flyspell-lazy-mode)))

(package! flyspell-correct-ivy :auto
  ;; Flyspell interface. Use M-o to access minibuffer actions
  :after flyspell
  :commands flyspell-correct-ivy
  :custom (flyspell-correct-interface #'flyspell-correct-ivy))


;; parens

(package! smartparens :auto
  :after (general) :defer 5
  :functions (show-smartparens-global-mode
              sp-kill-sexp sp-local-pair
              sp-local-pairs sp-pair
              sp-up-sexp)
  :after (evil general)
  :hook ((after-init . smartparens-global-mode))
  :init
  ;; fix highlighting in normal mode
  (setq sp-show-pair-from-inside t)
  (smartparens-global-mode +1)
  :config
  ;; from spacemacs
  (defun aero/smart-closing-parenthesis ()
    "Insert a closing pair delimiter or move point past existing delimiter.

If the expression at point is already balanced and there is a closing delimiter
for that expression on the current line, move point forward past the closing
delimiter. If the expression is balanced but there is no closing delimiter on
the current line, insert a literal ')' character. If the expression is not
balanced, insert a closing delimiter for the current expression. This command
uses Smartparens navigation commands and therefore recognizes pair delimiters
that have been defined using `sp-pair' or `sp-local-pair'."
    (interactive)
    (let* ((sp-navigate-close-if-unbalanced t)
           (current-pos (point))
           (current-line (line-number-at-pos current-pos))
           next-pos next-line)
      (aero/voidvar! sp-navigate-close-if-unbalanced)
      (save-excursion
        (let ((buffer-undo-list)
              (modified (buffer-modified-p)))
          (unwind-protect
              (progn
                (sp-up-sexp)
                (setq next-pos (point)
                      next-line (line-number-at-pos)))
            (primitive-undo (length buffer-undo-list)
                            buffer-undo-list)
            (set-buffer-modified-p modified))))
      (cond
       ((and (= current-line next-line)
             (not (= current-pos next-pos)))
        (sp-up-sexp))
       (t
        (insert-char ?\))))))

  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  (defun aero/copy-sexp-as-kill (&optional arg)
    "Copy the sexp to the kill ring without killing."
    (interactive)
    (funcall #'sp-kill-sexp arg t))

  (defun aero/sp-wrap-double-quote () (interactive) (sp-wrap-with-pair "\""))
  (defun aero/sp-wrap-single-quote () (interactive) (sp-wrap-with-pair "'"))
  (defun aero/sp-wrap-backtick () (interactive) (sp-wrap-with-pair "`"))
  (defun aero/sp-wrap-angle () (interactive) (sp-wrap-with-pair "<"))

  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "s^" '(sp-beginning-of-sexp :which-key "beginning of sexp")
   "s$" '(sp-end-of-sexp :which-key "end of sexp")
   "sh" '(sp-backward-sexp :which-key "back")
   "sl" '(sp-forward-sexp :which-key "forward")
   "sw" '(:ignore t :which-key "wrap")
   "sw(" 'sp-wrap-round
   "sw{" 'sp-wrap-curly
   "sw[" 'sp-wrap-square
   "sw<" 'aero/sp-wrap-angle
   "sw\"" '(aero/sp-wrap-double-quote :wk "wrap double quote")
   "sw'" '(aero/sp-wrap-single-quote :wk "wrap single quote")
   "sw`" '(aero/sp-wrap-backtick :wk "wrap backtick")
   "swr" 'sp-rewrap-sexp
   "su" '(sp-unwrap-sexp :which-key "unwrap")
   "sk" '(sp-kill-sexp :which-key "kill")
   "sK" '(aero/copy-sexp-as-kill :wk "copy as kill"))

  (sp-local-pair 'web-mode "<?" "?>")
  (sp-local-pair 'web-mode "<? " " ?>")
  (sp-local-pair 'web-mode "{" "}")
  (sp-local-pair 'web-mode "{ " " }")
  (sp-local-pair 'web-mode "{%" "%}")
  (sp-local-pair 'web-mode "{% " " %}")
  (sp-local-pair 'web-mode "`" "`")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-local-pair 'markdown-mode "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

  (sp-pair "<" ">")
  (sp-pair "< " " >")
  (sp-pair "{ " " }")
  (sp-pair "( " " )")
  (sp-pair "[ " " ]")

  ;; For these pairs, when hitting RET inside them, we add an extra newline to the middle and indent
  ;; accordingly.
  (sp-pair "{" "}" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "[" "]" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "(" ")" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "{ " " }" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "[ " " ]" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "( " " )" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "/** " " */" :post-handlers '(:add ("* ||\n[i]" "RET")))
  (sp-pair "/*" "*/" :post-handlers '(:add ("* ||\n[i]" "RET")))
  (sp-pair "/* " " */" :post-handlers '(:add ("* ||\n[i]" "RET")))

  (define-key evil-insert-state-map ")" 'aero/smart-closing-parenthesis))

(package! rainbow-delimiters :auto
  :hook ((prog-mode . rainbow-delimiters-mode)))


;;; formatting

(package! apheleia :auto
  :after general
  :config
  ;; For some reason, prettier won't read the config file from package.json. I'm just hard-coding
  ;; the config here because I'm done with the day. This will eventually come back to bite me, but
  ;; that's future-me's problem.
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier" "--single-quote" "--trailing-comma" "all" "--print-width" "110" input))
  (setf (alist-get 'prettier-typescript apheleia-formatters)
        '(npx "prettier" "--stdin-filepath" filepath "--parser=typescript" "--single-quote" "--trailing-comma" "all" "--print-width" "110"))

  ;; By default Apheleia tries to use elm-format directly, but I'd prefer to use npx
  (setf (alist-get 'elm-format apheleia-formatters)
        '(npx "elm-format" "--yes" "--stdin"))

  (add-to-list 'apheleia-formatters '(cljfmt "lein" "cljfmt" "fix" input))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljfmt))

  (aero-leader-def
    "bI" 'apheleia-format-buffer)
  (apheleia-global-mode +1))


;;; auto modes and stuff

(add-to-list 'auto-mode-alist '("\\(README\\|readme\\)\\'" . text-mode))
;; Use text mode for file that doesn't have an extension.
(add-to-list 'auto-mode-alist '("/[^./]*\\'" . text-mode))
;; Use conf-mode for dotfiles.
(add-to-list 'auto-mode-alist '("/\\.[^/]*\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.dir-locals\\.el\\'" . emacs-lisp-mode))

;; somehow makefile-mode stopped activating?
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq comment-auto-fill-only-comments t)
            (auto-fill-mode 1)))


;;; whitespace and indentation and stuff

(package! ws-butler :auto
  :functions (ws-butler-global-mode)
  :init (ws-butler-global-mode)
  :custom
  ;; default is just markdown-mode, which is a mode where I really want this in particular. Instead,
  ;; only exempt modes where whitespace could be important.
  (ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode)))

;; Ocaml
(package! tuareg :auto :mode ("\\.mli?\\'" . tuareg-mode))

;; SQL
(package! sql :builtin
  :defer t
  :after (general)
  :commands (sql-connect)
  :init
  (aero-leader-def
    "Sc" 'sql-connect)

  :config
  (setq sql-sqlite-program "sqlite3")

  (aero-mode-leader-def
    :keymaps 'sql-mode-map
    "b" 'sql-send-buffer
    "B" 'aero/sql-send-buffer-and-focus
    "r" 'sql-send-region
    "R" 'aero/sql-send-region-and-focus
    "p" 'sql-send-paragraph
    "P" 'aero/sql-send-paragraph-and-focus
    "s" 'sql-send-string
    "S" 'aero/sql-send-string-and-focus)

  ;; for sql comint
  (add-to-list 'same-window-buffer-names "*SQL: *")
  (add-hook 'sql-interactive-mode-hook #'evil-insert-state)

  (defun aero/sql-send-string-and-focus ()
    "Send a string to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (call-interactively 'sql-send-string)
      (evil-insert-state)))

  (defun aero/sql-send-buffer-and-focus ()
    "Send the buffer to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-buffer)
      (evil-insert-state)))

  (defun aero/sql-send-paragraph-and-focus ()
    "Send the paragraph to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-paragraph)
      (evil-insert-state)))

  (defun aero/sql-send-region-and-focus (start end)
    "Send region to SQLi and switch to SQLi in `insert state'."
    (interactive "r")
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-region start end)
      (evil-insert-state)))

  (defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))
  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook))

;; Docker

(package! docker-compose-mode :auto :mode "docker-compose.*\.yml\\'")
(package! dockerfile-mode :auto :mode "Dockerfile[a-zA-Z.-]*\\'")


;;; additional packages which might not fit elsewhere

(package! nix-mode :auto :mode "\\.nix\\'")
(package! lua-mode :auto :mode "\\.lua\\'")
(package! applescript-mode :auto :mode "\\.applescript\\'")
(package! nhexl-mode :auto :defer t) ; improved version of `hexl-mode'
(package! pdf-tools :auto :defer t)
(package! terraform-mode :auto :mode "\\.tf\\'")
(package! glsl-mode (:host github :repo "jimhourihan/glsl-mode")
  :mode "\\.\\(vert\\|frag\\)\\'"
  :config (add-hook 'glsl-mode-hook #'(lambda () (setq tab-width 4 c-basic-offset 4))))
(package! graphql-mode :auto :mode "\\.graphql\\'")
(package! groovy-mode :auto :mode "\\(\\.groovy\\'\\|Jenkinsfile\\)")
(package! csv-mode :auto :mode "\\.csv\\'")
(package! logstash-conf :auto :commands (logstash-conf-mode))

;; Elm-mode is supposed to enable elm-indent-mode by default, but for some reason it stopped doing
;; this on Dec 21, 2022. Probably caused this myself, but this fixes it.
(package! elm-mode :auto :hook (elm-mode . elm-indent-mode))

(provide 'aero-prog)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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

;; Standard completions library
(package! company
  (:repo "company-mode/company-mode"
   :files (:defaults "icons" ("images/small" "doc/images/small/*.png")))
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

;; Move commonly-used completions to the top
(package! company-prescient
  (:host github
   :repo "radian-software/prescient.el"
   :files ("company-prescient.el"))
  :after (company)
  :hook (company-mode . company-prescient-mode)
  :custom (prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir))
  :config (prescient-persist-mode +1))

;; Better popup interface for company
(package! company-box
  (:repo "sebastiencs/company-box" :files (:defaults "images"))
  :hook (company-mode . company-box-mode))


;; LSP

(package! eglot :builtin
  :hook ((python-mode
          python-ts-mode
          clojure-mode
          typescript-mode
          typescript-ts-mode
          js-mode
          js-ts-mode)
         . eglot-ensure)
  :after (general project)

  :custom
  (eglot-confirm-server-initiated-edits nil) ; don't ask to edit file immediately after I told it to
  (eglot-autoshutdown t) ; shutdown server after killing last managed buffer
  (eglot-events-buffer-size 0) ; disable event logging
  (eglot-send-changes-idle-time 0.75)
  ;; LSP highlighting is ridiculously slow, we use highlight-thing instead
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))

  :config
  ;; Individual server configuration
  (setq-default eglot-workspace-configuration
                '(:pylsp (:plugins (:pycodestyle (:enabled :json-false)
                                    ;; :pyflakes (:enabled :json-false)
                                    :pyls_mypy (:enabled t
                                                :live_mode :json-false)
                                    :pyls_black (:enabled t)
                                    :pyls_isort (:enabled t)))))

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

;; Optimizations to Eglot, using emacs-lsp-booster under the hood. emacs-lsp-booster must have been
;; installed already (its a Rust binary), which can be done with `make install-deps' or the more
;; specific `make lsp-booster'
(package! eglot-booster "jdtsmith/eglot-booster"
  :after eglot
  :config (eglot-booster-mode))

;; Make eglot send more info to eldoc, including parameter and function documentation
(package! eglot-signature-eldoc-talkative
  (:host codeberg :repo "mekeor/eglot-signature-eldoc-talkative" :branch "default")
  :after (eglot)
  :config (advice-add #'eglot-signature-eldoc-function :override #'eglot-signature-eldoc-talkative))

;; puts eldoc in a child frame instead of the echo area
(package! eldoc-box (:repo "casouri/eldoc-box")
  :after general

  :preface
  (defun aero/eldoc-set-documentation-strategy ()
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose))

  (defun aero/eldoc-box-help-at-point ()
    (interactive)
    (if (and (fboundp 'eglot-managed-p) (eglot-managed-p))
        (call-interactively #'eldoc-box-eglot-help-at-point)
      (call-interactively #'eldoc-box-help-at-point)))

  ;; Fix documentation strategy to show all of the available eldoc information when we want it. This
  ;; way Flymake errors don't just get clobbered by docstrings.
  :hook ((eglot-managed-mode . aero/eldoc-set-documentation-strategy)
         (prog-mode . eldoc-box-hover-mode))

  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-box-only-multi-line t) ; leave single-line docs in minibuffer
  (eldoc-box-max-pixel-width 600)
  (eldoc-box-max-pixel-height 600)

  :config
  (aero-leader-def
    "i" 'aero/eldoc-box-help-at-point
    "li" 'eldoc-box-eglot-help-at-point))


;; C language

(package! cc-mode :builtin
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

(package! markdown-mode "jrblevin/markdown-mode"
  :after (general smartparens)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode))
  :hook (markdown-mode . flyspell-mode)

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
  (markdown-fontify-code-blocks-natively t)

  :config
  ;; Don't expand checkboxes
  (sp-local-pair 'gfm-mode "- [ " "]"))

(package! markdown-toc (:repo "ardumont/markdown-toc")
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(package! yaml-mode (:repo "yoshiki/yaml-mode")
  :mode "\\.ya?ml\\'")


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
  ;; Buffer diagnostics in bottom window
  (add-to-list 'display-buffer-alist
               '("\\*Flymake diagnostics for.*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 23)))

  (aero-leader-def
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "eb" 'flymake-show-buffer-diagnostics))

;; makes flymake appear in popup
(package! flymake-diagnostic-at-point (:repo "meqif/flymake-diagnostic-at-point")
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(package! flymake-eslint "orzechowskid/flymake-eslint"
  :after (eglot)
  :init
  ;; Need to add after eglot so eglot doesn't clobber
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (and (or (derived-mode-p 'typescript-mode)
                             (derived-mode-p 'js-mode)
                             (derived-mode-p 'web-mode))
                         (not (derived-mode-p 'json-mode)))
                (flymake-eslint-enable)))))

(package! flyspell :builtin
  :after (general)
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :config
  (defvar aero-etc-dir)
  (setq
   flyspell-issue-message-flag nil
   ispell-personal-dictionary (expand-file-name "ispell/personal_dictionary.aws" aero/thornlog-path)
   flyspell-sort-corrections nil)

  ;; Skip code inside org src blocks
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

  (aero-leader-def
    "psP" 'flyspell-prog-mode
    "psN" 'flyspell-goto-next-error
    "psw" 'flyspell-word
    "psb" 'flyspell-buffer
    "psr" 'flyspell-region))

(package! flyspell-lazy (:repo "rolandwalker/flyspell-lazy")
  :hook ((flyspell-mode . flyspell-lazy-mode)))

(package! flyspell-correct "d12frosted/flyspell-correct"
  :after (general flyspell)
  :config
  (aero-leader-def
    "psc" 'flyspell-correct-wrapper
    "psC" 'flyspell-correct-at-point
    "psp" 'flyspell-correct-previous
    "psn" 'flyspell-correct-next))


;; parentheses

(package! smartparens "Fuco1/smartparens"
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
  (sp-local-pair 'web-mode "{" "}")
  (sp-local-pair 'web-mode "{ " " }")
  (sp-local-pair 'web-mode "{%" "%}")
  (sp-local-pair 'web-mode "{% " " %}")
  (sp-local-pair 'web-mode "{{ " " }}")
  (sp-local-pair 'web-mode "`" "`")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-local-pair 'markdown-mode "```" "```" :post-handlers '(:add ("||\n[i]" "RET")))

  (sp-pair "<" ">")

  ;; For these pairs, when hitting RET inside them, we add an extra newline to the middle and indent
  ;; accordingly.
  (sp-pair "{" "}" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "[" "]" :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-pair "(" ")" :post-handlers '(:add ("||\n[i]" "RET")))

  (define-key evil-insert-state-map ")" 'aero/smart-closing-parenthesis))

(package! rainbow-delimiters "Fanael/rainbow-delimiters"
  :hook ((prog-mode . rainbow-delimiters-mode)))


;;; formatting & debugging

(package! apheleia
  (:repo "radian-software/apheleia"
   :files (:defaults ("scripts" "scripts/formatters")))
  :after general
  :init (apheleia-global-mode +1)
  :config
  (dolist (cmd `((elm-format . (npx "elm-format" "--yes" "--stdin"))
                 (cljfmt . ("lein" "cljfmt" "fix" filepath))))
    (add-to-list 'apheleia-formatters cmd))

  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljfmt))

  (aero-leader-def
    "bI" 'apheleia-format-buffer))

;; Debug adapter, see https://github.com/svaante/dape for setup instructions, there's a lot of
;; manual setup required
(package! dape "svaante/dape"
  :after (general project eglot)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-adapter-dir (expand-file-name "debug-adapters/" user-emacs-directory))
  (dape-inlay-hints t)
  :config
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  (setq dape-configs-port 4711)

  (defun aero/install-vscode-js-debug ()
    "Run installation procedure to install JS debugging support"
    (interactive)
    (let ((vscode-js-debug-dir (expand-file-name "js-debug" dape-adapter-dir)))
      (mkdir vscode-js-debug-dir t)
      (let ((default-directory (expand-file-name vscode-js-debug-dir)))
        (vc-git-clone "https://github.com/microsoft/vscode-js-debug.git" "." nil)
        (message "git repository created")
        (call-process "npm" nil "*aero-install*" t "install")
        (message "npm dependencies installed")
        (call-process "npx" nil "*aero-install*" t "gulp" "dapDebugServer")
        (message "vscode-js-debug installed"))))

  ;; Set up JS
  (add-to-list 'dape-configs
               `(js-debug-node
                 modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 ;; Command to start the debug adapter
                 command "node"
                 command-cwd ,(concat dape-adapter-dir "js-debug")
                 command-args ("src/dapDebugServer.js" ,(format "%d" dape-configs-port))
                 ;; Port that Emacs will connect to the debug adapter
                 port ,dape-configs-port
                 ;; Debug configuration
                 :type "pwa-node"
                 :request "attach"
                 :name "Attach to Jest Test"
                 :address "localhost"
                 :port 9229 ;; Node.js debug port (from --inspect-brk)
                 :localRoot dape-cwd-fn
                 :remoteRoot nil
                 :skipFiles ["<node_internals>/**" "**/node_modules/**"]
                 :resolveSourceMapLocations ["!**/node_modules/**" "**/*"]
                 )))



;;; auto modes and stuff

(add-to-list 'auto-mode-alist '("\\(README\\|readme\\)\\'" . text-mode))
;; Use text mode for file that doesn't have an extension.
(add-to-list 'auto-mode-alist '("/[^./]*\\'" . text-mode))
(add-to-list 'auto-mode-alist '("/\\.dir-locals\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/Cask\\'" . emacs-lisp-mode))

;; somehow makefile-mode stopped activating?
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))


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
(package! glsl-mode (:repo "jimhourihan/glsl-mode")
  :mode "\\.\\(vert\\|frag\\)\\'"
  :config (add-hook 'glsl-mode-hook #'(lambda () (setq tab-width 4 c-basic-offset 4))))
(package! graphql-mode :auto :mode "\\.graphql\\'")
(package! groovy-mode :auto :mode "\\(\\.groovy\\'\\|Jenkinsfile\\)")
(package! csv-mode :auto :mode "\\.csv\\'")
(package! logstash-conf :auto :commands (logstash-conf-mode))

(add-hook
 'orson-mode-hook
 (lambda ()
   (setq-local indent-tabs-mode nil)
   (prettify-symbols-mode nil)))

;; Elm-mode is supposed to enable elm-indent-mode by default, but for some reason it stopped doing
;; this on Dec 21, 2022. Probably caused this myself, but this fixes it.
(package! elm-mode :auto :hook (elm-mode . elm-indent-mode))

(package! tcl :builtin
  :mode ("\\(\\.tcl\\|\\.test\\)\\'" . tcl-mode)
  :custom
  (tcl-application "tclsh")
  :config
  (add-to-list 'tcl-type-alist '("namespace" "eval" tcl-expr tcl-commands)))

(package! rivet-mode "thornjad/rivet-mode")

(provide 'aero-prog)

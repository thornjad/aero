;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2022 Jade Michael Thornton
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

(use-package company :straight t
  ;; Standard completions library
  :after (evil)
  :hook ((prog-mode . company-mode)
         (company-mode-hook . evil-normalize-keymaps))
  :init
  (setq company-idle-delay 0.5
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 15
        company-tooltip-margin 2
        company-require-match nil
        company-show-numbers t
	      company-tooltip-align-annotations t
        company-dabbrev-other-buffers t ; only look in open buffers with same major mode
        company-global-modes '(not erc-mode
                                   message-mode
                                   help-mode
                                   gud-mode
                                   vterm-mode))
  :config
  ;; allow eldoc trigger after completion
  (with-eval-after-load 'eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort)))

(use-package company-prescient :straight t
  ;; Move commonly-used completions to the top
  :after (company)
  :hook (company-mode . company-prescient-mode)
  :custom (prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir))
  :config (prescient-persist-mode +1))

(use-package company-box :straight t
  ;; Better popup interface for company
  :hook (company-mode . company-box-mode))

(use-package company-tabnine :straight t
  ;; Manages and provides Tabnine interface
  :after (company)
  :init (add-to-list 'company-backends #'company-tabnine))


;; LSP

(use-package eglot :straight t
  :hook (prog-mode . eglot-ensure)
  :after (general)
  :config
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

;; puts eldoc in a child frame. not enabled with eldoc because I'm not certain of it yet
(use-package eldoc-box :straight t
  :commands (eldoc-box-hover-mode))


;; C language

(use-package cc-mode :straight t
  :after flycheck
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . cpp-mode)
         ("\\.hpp\\'" . cpp-mode))
  :preface
  (defun aero/c-mode-common-hook ()
    "Hook to run in all C modes"
    (set (make-local-variable 'parens-require-spaces) nil))
  :hook (c-mode-common . aero/c-mode-common-hook)
  :config
  (add-to-list 'flycheck-disabled-checkers 'c/c++-clang))


;; Markup

(use-package markdown-mode :straight t
  :after (general)
  :commands (markdown-mode gfm-mode)
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))

  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh"))
  (add-hook 'markdown-mode-hook #'flyspell-mode)

  :config
  (require 'aero-thornlog)
  (aero-mode-leader-def
    :keymaps 'markdown-mode-map
    "t" 'today
    "d" 'new-day))

(use-package markdown-toc :straight t
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(use-package yaml-mode :straight t :mode "\\.ya?ml\\'")

(use-package org :straight nil
  :commands org-mode
  :config
  (setq org-src-preserve-indentation t
	      org-footnote-auto-adjust t
	      org-footnote-section nil
	      org-startup-with-inline-images t
	      org-startup-indented t)

  ;; rescale images to 400px if no with attribute is set (see
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
  (setq org-image-actual-width '(400))

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  ;; start with all levels collapsed
  (add-hook 'org-mode-hook #'org-hide-block-all))


;; flymake/flycheck

(use-package flymake :straight (:type built-in)
  :after (general)
  :config
  ;; left-fringe is the default, but we're being explicit because git-gutter also uses left-fringe.
  ;; Usually this works itself out.
  (setq flymake-fringe-indicator-position 'left-fringe
        flymake-wrap-around t)

  (aero-leader-def
   "en" 'flymake-goto-next-error
   "ep" 'flymake-goto-prev-error
   "eb" 'flymake-show-buffer-diagnostics))

;; makes flymake appear in popup
(use-package flymake-diagnostic-at-point
  :straight (:host github :repo "meqif/flymake-diagnostic-at-point")
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package flyspell
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

  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
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

(use-package flyspell-lazy
  :straight (:host github :repo "rolandwalker/flyspell-lazy")
  :hook ((flyspell-mode . flyspell-lazy-mode)))

(use-package flyspell-correct-ivy :straight t
  ;; Flyspell interface. Use M-o to access minibuffer actions
  :after flyspell
  :commands flyspell-correct-ivy
  :custom (flyspell-correct-interface #'flyspell-correct-ivy))

(use-package synosaurus :straight t
  ;; Thesaurus
  :after (general)
  :commands (synosaurus-lookup synosaurus-choose-and-replace)
  :custom (synosaurus-choose-method 'default)
  :config (aero-leader-def
            "tt" '(synosaurus-choose-and-replace :wk "synonyms")
            "tT" 'synosaurus-lookup))


;; parens

(use-package smartparens :straight t :after (general) :defer 5
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

(use-package rainbow-delimiters :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))


;;; formatting

(use-package apheleia :straight t
  :config
  ;; For some reason, prettier won't read the config file from package.json. I'm just hard-coding
  ;; the config here because I'm done with the day. This will eventually come back to bite me, but
  ;; that's future-me's problem.
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier" "--single-quote" "--trailing-comma" "all" "--print-width" "110" input))
  (apheleia-global-mode +1))


;;; auto modes and stuff

(add-to-list 'auto-mode-alist '("\\(README\\|readme\\)\\'" . text-mode))
;; Use text mode for file that doesn't have an extension.
(add-to-list 'auto-mode-alist '("/[^./]*\\'" . text-mode))
;; Use conf-mode for dotfiles.
(add-to-list 'auto-mode-alist '("/\\.[^/]*\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.dir-locals\\.el\\'" . emacs-lisp-mode))

;; somehow makefile-mode broke??
(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))

(add-hook 'prog-mode-hook (lambda ()
                            (setq comment-auto-fill-only-comments t)
                            (auto-fill-mode 1)))

(defun aero/strip-namespace-from-xref (arg)
  "Remove namespace qualifiers from xref call."
  ;; Split up by mode because qualifiers are different all over
  (cond
   ((equal major-mode "tcl-mode")
    (last (split-string arg "::")))))


;;; whitespace and indentation and stuff

(use-package ws-butler :straight t
  :functions (ws-butler-global-mode)
  :init (ws-butler-global-mode)
  :custom
  ;; default is just markdown-mode, which is a mode where I really want this in particular. Instead,
  ;; only exempt modes where whitespace could be important.
  (ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode)))


;;; additional packages which might not fit elsewhere

(use-package nix-mode :straight t :mode "\\.nix\\'")
(use-package lua-mode :straight t :mode "\\.lua\\'")
(use-package applescript-mode :straight t :mode "\\.applescript\\'")
(use-package nhexl-mode :straight t :defer t) ; improved version of `hexl-mode'
(use-package pdf-tools :straight t :defer t)
(use-package terraform-mode :straight t :mode "\\.tf\\'")
(use-package glsl-mode :straight (:host github :repo "jimhourihan/glsl-mode") :mode "\\.\\(vert\\|frag\\)\\'")
(use-package graphql-mode :straight t :mode "\\.graphql\\'")
(use-package groovy-mode :straight t)

;; Ocaml
(use-package tuareg :straight t :mode ("\\.mli?\\'" . tuareg-mode))

(provide 'aero-prog)

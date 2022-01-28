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
  :after (evil)
	:hook ((prog-mode . company-mode)
         (company-mode-hook . evil-normalize-keymaps))
	:init
	(setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-dabbrev-downcase nil
        company-tooltip-limit 10
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
  :after (company)
	:hook (company-mode . company-prescient-mode)
	:defines (prescient-save-file)
	:commands (prescient-persist-mode)
	:config
  ;; duplicate setq with company-prescient
	(setq prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir)))

(use-package company-box :straight t
  :hook (company-mode . company-box-mode))

(use-package company-dict :straight t
  :after (company flyspell)
  :config
  (setq company-dict-dir (expand-file-name "ispell" aero-etc-dir)))

(use-package counsel-gtags
  :after (general)
  :commands (counsel-gtags-dwim)
  :init
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "]" 'counsel-gtags-dwim))

(use-package company-tabnine :straight t
  :after (company)
  :init (add-to-list 'company-backends #'company-tabnine))


;; C lang

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
  (add-hook 'markdown-mode-hook 'variable-pitch-mode)

  (require 'aero-thornlog)
  (aero-mode-leader-def
    :keymaps 'markdown-mode-map
    "t" 'today
    "d" 'new-day))

(use-package markdown-toc :straight t
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(use-package yaml-mode :straight t
  :mode "\\.ya?ml\\'")

(use-package clue :defer t
  :straight (:host github :repo "AmaiKinono/clue")
  :after (general)
  :hook (find-file-hook . clue-auto-enable-clue-mode)
  :commands (clue-copy
             clue-paste)
  :custom
  (clue-project-root-function #'projectile-project-root)
  (clue-auto-enable-modes '(markdown-mode))
  :init
  (aero-leader-def
    "Cc" 'clue-copy
    "Cp" 'clue-paste))

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


;; ML

(use-package tuareg :straight t
  :mode ("\\.mli?\\'" . tuareg-mode))


;; Java/Clojure/Groovy

(use-package clojure-mode :straight t
  :mode "\\.clj\\'")
(use-package cider :straight t
  :hook (clojure-mode . cider-mode)
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

(use-package groovy-mode :straight t)


;; flycheck

(defvar flycheck-idle-change-delay 3.0)
(make-variable-buffer-local 'flycheck-idle-change-delay)
(use-package flycheck :after (general)
  :commands flycheck-mode
  :functions (flycheck-buffer-automatically)
  :hook ((web-mode
          tcl-mode
          json-mode
          js2-mode
          emacs-lisp-mode
          c-mode
          cpp-mode)
         . flycheck-mode)
  :init
  (dolist (where '((emacs-lisp-mode-hook . emacs-lisp-mode-map)
                   (haskell-mode-hook    . haskell-mode-map)
                   (js2-mode-hook        . js2-mode-map)
                   (c-mode-common-hook   . c-mode-base-map)))
    (add-hook (car where)
              `(lambda ()
                 (bind-key "M-n" #'flycheck-next-error ,(cdr where))
                 (bind-key "M-p" #'flycheck-previous-error ,(cdr where)))))
  :config
  (setq flycheck-highlighting-mode 'symbols)
  (defalias 'show-error-at-point-soon 'flycheck-show-error-at-point)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  (defun aero/auto-adjust-flycheck-eagerness ()
    "Adjust how often we check for errors based on if there are any.
    In a clean, error-free buffer, we're an order of magnitude more
    lax about running the checks."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 0.3 3.0)))
  ;; auto-adjust at the buffer level
  (add-hook 'flycheck-after-syntax-check-hook
            'aero/auto-adjust-flycheck-eagerness)

  (eval-when-compile
    (declare-function flycheck-clear-idle-change-timer "flycheck"))
  (defun flycheck-handle-idle-change ()
    "Handle an expired idle time since the last change. This is an
  overwritten versioon of the original flycheck-handle-idle-change,
  which removes the forced deferred. Timers should only trigger
  inbetween commands in a single threaded system and the forced
  deferred makes errors never show up before you execute another
  command. Credit to jwiegley for this one"
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change))

  (general-define-key
   :states 'normal
   :prefix "SPC"
   "pc" '(:ignore t :which-key "flycheck")
   "pcn" 'flycheck-next-error
   "pcp" 'flycheck-previous-error
   "pcc" 'flycheck-clear
   "pcb" 'flycheck-buffer
   "pcy" '(flycheck-copy-errors-as-kill :which-key "yank as kill")
   "pch" 'flycheck-display-error-at-point
   "pce" 'flycheck-explain-error-at-point
   "pcl" 'flycheck-list-errors
   "pcH" 'flycheck-manual))

(use-package flycheck-pos-tip :straight t
  :hook (flycheck-mode . flycheck-pos-tip-mode))

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
                               aero-etc-dir))

  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

	(use-package flyspell-correct-popup :straight t
		:commands flyspell-correct-wrapper
    :functions (flyspell-correct-popup)
		:init
    (defvar flyspell-correct-interface)
		(setq flyspell-correct-interface #'flyspell-correct-popup))

  (declare-function aero-leader-def "aero-prelude.el")
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

;;; parens

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

  (defun aero/smartparens-pair-newline-and-indent ()
    "Insert newline after smart paren, then indent for insert."
    (save-excursion
      (newline)
      (indent-according-to-mode))
    (indent-according-to-mode))

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
   "su" '(sp-unwrap-sexp :which-key "unwrap")
   "sk" '(sp-kill-sexp :which-key "kill")
   "sK" '(aero/copy-sexp-as-kill :wk "copy as kill"))

  (sp-local-pair 'web-mode "<?" "?>")
  (sp-local-pair 'web-mode "{" "}")
  (sp-local-pair 'web-mode "`" "`")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-pair "{" nil :post-handlers
           '(:add (aero/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
           '(:add (aero/smartparens-pair-newline-and-indent "RET")))
  (define-key evil-insert-state-map ")" 'aero/smart-closing-parenthesis))

(use-package rainbow-delimiters :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))


(add-to-list 'auto-mode-alist '("\\(README\\|readme\\)\\'" . text-mode))

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
	:config
  (defvar ws-butler-global-exempt-modes)
	(setq ws-butler-global-exempt-modes
				(append ws-butler-global-exempt-modes
								'(special-mode comint-mode term-mode eshell-mode))))

(use-package indent-indicator
  :straight (:host gitlab :repo "thornjad/indent-indicator")
	:after (general)
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "bI" 'indent-indicator-mode))


;;; additional packages which might not fit elsewhere

(use-package nix-mode :straight t :mode "\\.nix\\'")
(use-package lua-mode :straight t :mode "\\.lua\\'")

;; Improved version of `hexl-mode' for editing hex/binary
(use-package nhexl-mode :straight t :defer t)

(use-package applescript-mode :straight t)

(use-package smartscan :straight t
  :after (general)
  :hook (prog-mode . smartscan-mode))

(use-package yasnippet :straight t
  :after (general)
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets/" aero-etc-dir)))
  :config
  ;; drop the default keys
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; Better, not so overloaded key
  (define-key yas-minor-mode-map (kbd "C-<tab>") yas-maybe-expand)
  (define-key yas-minor-mode-map (kbd "C-TAB") yas-maybe-expand)

  (aero-leader-def
    "hdy" 'yas-describe-tables
    "ty" 'yas-insert-snippet)

  (yas-global-mode +1))

(provide 'aero-prog)

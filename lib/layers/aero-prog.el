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


;; Java/Clojure/Groovy

(use-package clojure-mode :straight t :mode "\\.clj\\'")
(use-package groovy-mode :straight t)
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


;; flycheck

(defvar flycheck-idle-change-delay 3.0)
(make-variable-buffer-local 'flycheck-idle-change-delay)
(use-package flycheck :after (general)
  :commands flycheck-mode
  :functions (flycheck-buffer-automatically)
  :hook ((web-mode tcl-mode json-mode js2-mode emacs-lisp-mode c-mode cpp-mode)
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

  ;; boot flycheck to the right fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  ;; display errors in a lower buffer and auto-resize it
  (add-to-list 'display-buffer-alist
               (cons
                (rx string-start (eval flycheck-error-list-buffer) string-end)
                '(display-buffer-below-selected
                  . ((window-height . shrink-window-if-larger-than-buffer)
                     (reusable-frames . t)))))
  (defadvice flycheck-error-list-refresh (around shrink-error-list activate)
    ad-do-it
    (-when-let (window (flycheck-get-error-list-window t))
      (with-selected-window window
        (fit-window-to-buffer window 30 10))))

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

;; Display flycheck error in tooltip at point
(use-package flycheck-popup-tip :straight t
  :after (flycheck)
  :hook (flycheck-mode . flycheck-popup-tip-mode))

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
  (sp-pair "/**" "*/" :post-handlers '(:add ("* ||\n[i]" "RET")))
  (sp-pair "/*" "*/" :post-handlers '(:add ("* ||\n[i]" "RET")))

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
	:config
  (defvar ws-butler-global-exempt-modes)
	(setq ws-butler-global-exempt-modes
				(append ws-butler-global-exempt-modes
								'(special-mode comint-mode term-mode eshell-mode))))


;;; additional packages which might not fit elsewhere

(use-package nix-mode :straight t :mode "\\.nix\\'")
(use-package lua-mode :straight t :mode "\\.lua\\'")
(use-package applescript-mode :straight t :mode "\\.applescript\\'")
(use-package nhexl-mode :straight t :defer t) ; improved version of `hexl-mode'
(use-package pdf-tools :straight t :defer t)
(use-package terraform-mode :straight t :mode "\\.tf\\'")
(use-package glsl-mode :straight (:host github :repo "jimhourihan/glsl-mode") :mode "\\.\\(vert\\|frag\\)\\'")

;; Ocaml
(use-package tuareg :straight t :mode ("\\.mli?\\'" . tuareg-mode))

(provide 'aero-prog)

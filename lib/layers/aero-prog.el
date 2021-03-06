;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2021 Jade Michael Thornton
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
        company-tooltip-limit 14
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

(use-package company-box :straight t
  :after (company)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-max-candidates 50
        company-box-icons-alist 'company-box-icons-all-the-icons
        ;; company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-box-icons-all-the-icons
        (let ((all-the-icons-scale-factor 0.8))
          `((Unknown . ,(all-the-icons-material "find_in_page" :face 'all-the-icons-purple))
            (Text . ,(all-the-icons-material "text_fields" :face 'all-the-icons-green))
            (Method . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Function . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Constructor . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Field . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (Variable . ,(all-the-icons-material "adjust" :face 'all-the-icons-blue))
            (Class . ,(all-the-icons-material "class" :face 'all-the-icons-red))
            (Interface . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module . ,(all-the-icons-material "view_module" :face 'all-the-icons-red))
            (Property . ,(all-the-icons-material "settings" :face 'all-the-icons-red))
            (Unit . ,(all-the-icons-material "straighten" :face 'all-the-icons-red))
            (Value . ,(all-the-icons-material "filter_1" :face 'all-the-icons-red))
            (Enum . ,(all-the-icons-material "plus_one" :face 'all-the-icons-red))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :face 'all-the-icons-red))
            (Snippet . ,(all-the-icons-material "short_text" :face 'all-the-icons-red))
            (Color . ,(all-the-icons-material "color_lens" :face 'all-the-icons-red))
            (File . ,(all-the-icons-material "insert_drive_file" :face 'all-the-icons-red))
            (Reference . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
            (Folder . ,(all-the-icons-material "folder" :face 'all-the-icons-red))
            (EnumMember . ,(all-the-icons-material "people" :face 'all-the-icons-red))
            (Constant . ,(all-the-icons-material "pause_circle_filled" :face 'all-the-icons-red))
            (Struct . ,(all-the-icons-material "streetview" :face 'all-the-icons-red))
            (Event . ,(all-the-icons-material "event" :face 'all-the-icons-red))
            (Operator . ,(all-the-icons-material "control_point" :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class" :face 'all-the-icons-red))
            (Template . ,(all-the-icons-material "short_text" :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle" :face 'all-the-icons-blue))
            (ElispFeature . ,(all-the-icons-material "stars" :face 'all-the-icons-orange))
            (ElispFace . ,(all-the-icons-material "format_paint" :face 'all-the-icons-pink))))
        x-gtk-resize-child-frames 'resize-mode)

  ;; disable tab bar in company-box child frames. Will probably be PR'd upstream eventually
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0)))

(use-package company-prescient :straight t
  :after (company)
	:hook (company-mode . company-prescient-mode)
	:defines (prescient-save-file)
	:commands (prescient-persist-mode)
	:config
  ;; duplicate setq with company-prescient
	(setq prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir)))

(use-package company-dict :straight t
  :after (company flyspell)
  :config
  (setq company-dict-dir (expand-file-name "ispell" aero-etc-dir)))

(use-package counsel-gtags :after (general)
  :commands (counsel-gtags-dwim)
  :init
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "]" 'counsel-gtags-dwim))


;;; flycheck

(defvar flycheck-idle-change-delay 3.0)
(make-variable-buffer-local 'flycheck-idle-change-delay)
(use-package flycheck :after (general)
  :commands flycheck-mode
  :functions (flycheck-buffer-automatically)
  :hook ((web-mode
          tcl-mode
          json-mode
          js2-mode
          rjsx-mode
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
  :after (evil)
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
  (sp-local-pair 'rjsx-mode "`" "`")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-pair "{" nil :post-handlers
           '(:add (aero/smartparens-pair-newline-and-indent "RET")))
  (sp-pair "[" nil :post-handlers
           '(:add (aero/smartparens-pair-newline-and-indent "RET")))
  (define-key evil-insert-state-map ")" 'aero/smart-closing-parenthesis))

(use-package siege-mode :straight (:host github :repo "tslilc/siege-mode")
  :after (general)
  :commands (siege-explicit-call)
  :config
  (aero-leader-def
    "sw RET" '(siege-explicit-call :wk "siege surround")))

(use-package rainbow-delimiters :straight t
  :hook ((prog-mode . rainbow-delimiters-mode)))


(add-to-list 'auto-mode-alist '("\\(README\\|readme\\)\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.\\(applescript\\)\\'" . prog-mode))

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

(defun aero/so-long-hook () "Used in `so-long-hook'.")
(when (require 'so-long nil :noerror)
  (defvar so-long-threshold)
  (defvar so-long-minor-modes)

  ;; 750 columns means it's too long! (default is 250)
  (setq so-long-threshold 750)
  (add-to-list 'so-long-minor-modes 'rainbow-delimiters-mode)
  (add-to-list 'so-long-minor-modes 'paren-face-mode)
  (add-to-list 'so-long-minor-modes 'electric-indent-mode)
  (add-to-list 'so-long-minor-modes 'electric-pair-mode)
  (add-to-list 'so-long-minor-modes 'electric-layout-mode)
  (add-to-list 'so-long-minor-modes 'idle-highlight-mode)
  (add-to-list 'so-long-minor-modes 'show-paren-mode)
  (add-to-list 'so-long-minor-modes 'git-gutter-mode)
  (global-so-long-mode +1)
  (add-hook 'so-long-hook #'aero/so-long-hook))


;;; additional packages which might not fit elsewhere

(use-package nix-mode :straight t :mode "\\.nix\\'")
(use-package lua-mode :straight t :mode "\\.lua\\'")

;; Improved version of `hexl-mode' for editing hex/binary
(use-package nhexl-mode :straight t :defer t)



;; Other tweaks



;;; end

(provide 'aero-prog)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(use-package company :straight t
	:hook (prog-mode . company-mode)
	:init
	(setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-require-match nil
        company-show-numbers t
				company-tooltip-align-annotations t))

(use-package counsel-gtags :straight t
  :commands (counsel-gtags-dwim)
  :init
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "]" 'counsel-gtags-dwim))


;;; flycheck

(defvar flycheck-idle-change-delay 3.0)
(make-variable-buffer-local 'flycheck-idle-change-delay)
(use-package flycheck :straight t
  :commands flycheck-mode
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
   "pcH" 'flycheck-display-error-at-point-soon
   "pce" 'flycheck-explain-error-at-point
   "pcl" 'flycheck-list-errors
   "pci" 'flycheck-manual))

;;; parens

(use-package smartparens :straight t
  :functions (sp-pair
              sp-local-pairs
              sp-up-sexp)
  :commands smartparens-global-mode
  :after evil
  :hook ((after-init . smartparens-global-mode))
  :init
  ;; fix highlighting in normal mode
  (setq sp-show-pair-from-inside t)
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

  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "s0" '(sp-beginning-of-sexp :which-key "beginning of sexp")
   "s$" '(sp-end-of-sexp :which-key "end of sexp")
   "sk" '(sp-up-sexp :which-key "up")
   "sj" '(sp-down-sexp :which-key "down")
   "sh" '(sp-backward-sexp :which-key "back")
   "sl" '(sp-forward-sexp :which-key "forward")
   "sw" '(:ignore t :which-key "wrap")
   "sw(" 'sp-wrap-round
   "sw{" 'sp-wrap-curly
   "sw[" 'sp-wrap-square
   "su" '(sp-unwrap-sexp :which-key "unwrap")
   "sK" '(sp-kill-sexp :which-key "kill"))

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
  (define-key evil-insert-state-map ")"
    'aero/smart-closing-parenthesis))


(add-to-list 'auto-mode-alist '("\\README\\'" . text-mode))

(defun aero/strip-namespace-from-xref (arg)
  "Remove namespace qualifiers from xref call."
  ;; Split up by mode because qualifiers are different all over
  (cond
   ((equal major-mode "tcl-mode")
    (last (split-string arg "::")))))


;;; whitespace and indentation

(eval-when-compile
  (defvar whitespace-mode)
  (defvar whitespace-style)
  (defvar global-whitespace-mode))
(defvar aero/did-have-whitespace-p nil)
(make-variable-buffer-local 'aero/did-have-whitespace-p)

(defvar aero/disable-modes-when-pseudo-tooltip
  '(whitespace-mode indent-guide-global-mode formfeeder-mode)
  "Modes to toggle when `company-mode''s pseudo-tooltip is active.")

(global-whitespace-mode)
(setq whitespace-style
      '(face
        tabs tab-mark
        spaces space-mark
        empty trailing))

(use-package indent-guide :straight t
  :init
  (indent-guide-global-mode 1))

(defun pre-popup-draw ()
  "Turn off whitespace and indent before showing company complete tooltip."
  (if (or whitespace-mode global-whitespace-mode)
      (progn
        (dolist (mode aero/disable-modes-when-pseudo-tooltip)
          (funcall mode -1))
        (setq aero/did-have-whitespace-p t))))
(defun post-popup-draw ()
  "Restore previous whitespace and indent after showing company tooltip."
  (if aero/did-have-whitespace-p
      (progn
        (dolist (mode aero/disable-modes-when-pseudo-tooltip)
          (funcall mode +1))
        (setq aero/did-have-whitespace-p nil))))

(eval-after-load "company"
  (progn
    (advice-add 'company-pseudo-tooltip-unhide :before #'pre-popup-draw)
    (advice-add 'company-pseudo-tooltip-hide :after #'post-popup-draw)))



(provide 'aero-prog)

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

(use-package company :ensure t
	:pin gnu
	:hook (prog-mode . company-mode)
	:init
	(setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers t
				company-tooltip-align-annotations t))

;; TODO bindings
(use-package counsel-gtags :ensure t)

(use-package smart-tabs-mode
  :load-path aero-packages-directory
  :config
  (smart-tabs-insinuate 'c 'c++ 'javascript 'python 'tcl))

(use-package flycheck :ensure t
  :commands flycheck-mode
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
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'aero/auto-adjust-flycheck-eagerness)

  (defun flycheck-handle-idle-change ()
    "Handle an expired idle time since the last change. This is an
  overwritten version of the original flycheck-handle-idle-change,
  which removes the forced deferred. Timers should only trigger
  inbetween commands in a single threaded system and the forced
  deferred makes errors never show up before you execute another
  command. Credit to jwiegley for this one"
    (flycheck-clear-idle-change-timer)
    (flycheck-buffer-automatically 'idle-change))

  ;; hooks for several modes
  (dolist (modehook '(web tcl json js2 rjsx emacs-lisp))
    (add-hook (concat (car modehook) "-mode-hook")
              'flycheck-mode)))


;;; parens

(use-package smartparens :ensure t
  :commands smartparens-global-mode
  :hook ((after-init . smartparens-global-mode))
  :config
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

  (sp-local-pair 'web-mode "<? " " ?>")
  (sp-local-pair 'web-mode "{ " " }")
  (sp-local-pair 'web-mode "{{ "  " }}")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" ))

(provide 'aero-prog)

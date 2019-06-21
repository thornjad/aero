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

(use-package smartparens :ensure t
  :commands smartparens-global-mode
  :hook ((after-init . smartparens-global-mode)
         (prog-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)

  (general-define-key
   :states 'normal
   :prefix "SPC"
   "s0" '(sp-beginning-of-sexp :which-key "beginning of sexp")
   "s$" '(sp-end-of-sexp :which-key "end of sexp")
   "sk" '(sp-up-sexp :which-key "up sexp")
   "sj" '(sp-down-sexp :which-key "down sexp")
   "sh" '(sp-backward-sexp :which-key "back sexp")
   "sl" '(sp-forward-sexp :which-key "forward sexp")
   "su" '(sp-unwrap-sexp :which-key "unwrap sexp")
   "sK" '(sp-kill-sexp :which-key "kill sexp"))

  (sp-local-pair 'web-mode "<? " " ?>")
  (sp-local-pair 'web-mode "{ " " }")
  (sp-local-pair 'web-mode "{{ "  " }}")
  (sp-local-pair 'org-mode "$" "$")
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" ))

(provide 'aero-prog)

;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
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

(require 'aero-prelude)

(use-package web-mode :ensure t
	:config
	(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.rvt\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
	(general-define-key
	 :states '(normal emacs)
	 :prefix "SPC"))

(use-package emmet-mode :ensure t
	:config
	(setq emmet-self-closing-tag-style " /")
	(aero/add-hook!
	 'rjsx-mode-hook
	 (set-face-attribute 'rjsx-attr nil :inherit font-lock-variable-name-face :slant 'italic)
	 (setq emmet-expand-jsx-className? t)))

(use-package scss-mode :ensure t
  :defer t
  :mode ("\\.scss\\.css\\'" . scss-mode))


;; js and jsx

(use-package js2-mode :ensure t)

(use-package rjsx-mode :ensure t
	:after js2-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

	;; try fixing indentation, not really working
	;; https://github.com/felipeochoa/rjsx-mode/issues/85
	(aero/add-hook!
	 'rjsx-mode-hook
	 (setq-local indent-line-function 'js-jsx-indent-line)
	 (setq index-tabs-mode t
				 c-basic-offset 2
				 tab-width 2)))


;; rivet

;; modified from configuration by James Sulak
(use-package mmm-mode :ensure t
	:config
	(require 'mmm-auto)
	(mmm-add-classes
	 '((web-rvt
			:submode tcl-mode
			:delimiter-mode nil
			:front "<\\?[=]?"
			:front-offset 1
			:back-offset 1
			:back "\\?>")))
	(setq mmm-submode-decoration-level 0)
	(setq mmm-global-mode 'maybe)
	(mmm-add-mode-ext-class 'web-mode "\\.rvt\\'" 'web-rvt)
	(setq auto-mode-alist (append (list (cons "\\.rvt\\'" 'web-mode))
																auto-mode-alist)))


;; the rest

(use-package coffee-mode :ensure t)

;; major mode for editing Apache configuration files
(use-package apache-mode :ensure t
  :quelpa (apache-mode :fetcher github :repo "emacsmirror/apache-mode"))

(provide 'aero-web)

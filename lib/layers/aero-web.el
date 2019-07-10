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

(use-package web-mode :ensure t
  :mode
  "\\.\\(tpl\\|php\\|xml\\|html?\\|djhtml\\|erb\\|eco\\|ejs\\)\\'")

(use-package emmet-mode :ensure t
  :defer t
  :hook ((web-mode html-mode css-mode scss-mode rjsx-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-line)
  (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-line)
  (evil-define-key 'hybrid emmet-mode-keymap (kbd "TAB") 'emmet-expand-line)
  (evil-define-key 'hybrid emmet-mode-keymap (kbd "<tab>") 'emmet-expand-line)

  (add-hook
   'rjsx-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :ensure t
  :mode "\\.s?css\\'"
  :ensure-system-package
  (sass-lint . "npm i -g sass-lint"))


;; js and jsx

(use-package rjsx-mode
  :load-path "lib/packages/rjsx-mode/"
  :mode "\\.jsx?\\'"

  ;; ensure flycheck can run properly
  :ensure-system-package
  ((eslint . "npm i -g eslint")
   (tern . "npm i -g tern"))

  :config
  ;; TODO make this more better
  (add-to-list
   'load-path
   "/Users/jade.thornton/.nvm/versions/node/v11.3.0/lib/node_modules/tern/emacs/")
  (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'rjsx-mode #'tern-mode)
  (add-hook 'rjsx-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))
  ;; FIXME something is resetting this when this mode loads, need to find out how/where
  (setq js2-basic-offset 2))

(use-package json-mode :ensure t
	:mode "\\.json\\'")

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

;; rivet

;; modified from configuration by James Sulak
(use-package mmm-mode :ensure t
  :mode "\\.\\(rvt\\|test\\)\\'"
  :functions (mmm-add-mode-ext-class)
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

(use-package coffee-mode :ensure t
  :mode "\\.coffee\\'"
  :ensure-system-package
  ((coffee . "npm i -g coffeescript")
   (coffeelint . "npm i -g coffeelint")))

(provide 'aero-web)

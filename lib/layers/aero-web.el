;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
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
;;; Code:

(use-package web-mode :ensure t
  :load-path "lib/packages/web-mode/"
  :mode
  "\\.\\(tpl\\|php\\|xml\\|html?\\|djhtml\\|erb\\|eco\\|ejs\\)\\'")

(use-package emmet-mode :ensure t
  :load-path "lib/packages/emmet-mode/"
  :hook ((web-mode html-mode css-mode scss-mode rjsx-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'rjsx-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :ensure t
  :load-path "lib/packages/emmet-mode"
  :mode "\\.s?css\\'"
  :ensure-system-package
  (sass-lint . "npm i -g sass-lint"))


;; js and jsx

(use-package js2-mode
  :load-path "lib/packages/js2-mode/"
  :defer t)
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
  (setq js2-basic-offset 2)

  ;; because we want C-d to scroll ups normally
  (evil-define-key 'insert rjsx-mode-map
    (kbd "C-d") 'rjsx-delete-creates-full-tag)
  (evil-define-key 'normal rjsx-mode-map
    (kbd "C-d") 'evil-scroll-down))

(use-package json-mode :ensure t
	:mode "\\.json\\'")

(use-package yaml-mode :ensure t
  :mode "\\.ya?ml\\'")

;; rivet

;; ;; modified from configuration by James Sulak
;; (use-package mmm-mode :ensure t
;;   :mode "\\.\\(rvt\\|test\\)\\'"
;;   :functions (mmm-add-mode-ext-class)
;; 	:config
;; 	(require 'mmm-auto)
;; 	(mmm-add-classes
;; 	 '((web-rvt
;; 			:submode tcl-mode
;; 			:delimiter-mode nil
;; 			:front "<\\?[=]?"
;; 			:front-offset 1
;; 			:back-offset 1
;; 			:back "\\?>")))
;; 	(setq mmm-submode-decoration-level 0)
;; 	(setq mmm-global-mode 'maybe)
;; 	(mmm-add-mode-ext-class 'web-mode "\\.rvt\\'" 'web-rvt)
;; 	(setq auto-mode-alist (append (list (cons "\\.rvt\\'" 'web-mode))
;; 																auto-mode-alist)))


;; the rest

(use-package coffee-mode :ensure t
  :mode "\\.coffee\\'"
  :ensure-system-package
  ((coffee . "npm i -g coffeescript")
   (coffeelint . "npm i -g coffeelint")))

(provide 'aero-web)

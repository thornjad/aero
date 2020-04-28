;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
;;
;; Copyright (c) 2018-2020 Jade Michael Thornton
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

(require 'aero-lib)

(use-package web-mode :straight t
  :load-path "lib/packages/web-mode/"
  :mode
  "\\.\\(tpl\\|php\\|xml\\|html?\\|djhtml\\|erb\\|eco\\|ejs\\|svg\\)\\'")

(use-package emmet-mode :straight t
  :load-path "lib/packages/emmet-mode/"
  :hook ((web-mode html-mode css-mode scss-mode rjsx-mode js-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'rjsx-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :straight t
  :load-path "lib/packages/emmet-mode"
  :mode "\\.s?css\\'")


;; js and jsx

(use-package js2-mode
  :load-path "lib/packages/js2-mode/"
  :defer t)

(use-package rjsx-mode
  :load-path "lib/packages/rjsx-mode/"
  :mode "\\.jsx?\\'"

  :config
  ;; because we want C-d to scroll up normally
  (evil-define-key 'insert rjsx-mode-map
    (kbd "C-d") 'rjsx-delete-creates-full-tag)
  (evil-define-key 'normal rjsx-mode-map
    (kbd "C-d") 'evil-scroll-down))

;; (use-package js :straight nil
;;   :mode ("\\.jsx?\\'" . js-mode))

(eval-when-compile (defvar emmet-expand-jsx-className?))
(add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))

(use-package json-mode :straight t
	:mode "\\.json\\'")

(use-package yaml-mode :straight t
  :mode "\\.ya?ml\\'")

;; the rest

(use-package coffee-mode :straight t
  :mode "\\.coffee\\'")

(use-package restclient
	:straight t
  :defer t
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)

  :init
  (aero-leader-def
    "wR" 'restclient-mode
    "wr" 'aero/restclient-scratch)

  :config
  (use-package company-restclient :straight t
    :config
    (add-to-list 'company-backends 'company-restclient))

  (aero-mode-leader-def
   :keymaps 'restclient-mode-map
    "RET" '(restclient-http-send-current-stay-in-window :wk "Run query at point")
    "c" '(restclient-http-send-current :wk "Run query at point and focus")
    "r" '(restclient-http-send-current-raw :wk "Run query, no pretty print")
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "." 'restclient-mark-current
    "y" 'restclient-copy-curl-command))

(provide 'aero-web)

;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
;;
;; Copyright (c) 2018-2021 Jade Michael Thornton
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
(require 'aero-prelude)

(use-package web-mode :straight t
  :mode
  "\\.\\(tpl\\|php\\|xml\\|html?\\|djhtml\\|erb\\|eco\\|ejs\\|svg\\)\\'"
  :config
  (setq web-mode-engines-alist
        '(("ctemplate" . "\\.tpl\\'"))))

(use-package emmet-mode :straight t
  :load-path "lib/packages/emmet-mode/"
  :hook ((web-mode html-mode css-mode scss-mode rjsx-mode js-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'rjsx-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t)))
  (add-hook
   'js-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :straight t
  :mode "\\.s?css\\'")


;; js and jsx

(defun node-repl ()
  "Launch a Node.js comint REPL."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")  ; avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(eval-when-compile (defvar emmet-expand-jsx-className?))
(add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))

(use-package json-mode :straight t
	:mode "\\.json\\'")


;; the rest

(use-package restclient :defer t
	:after (general)
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (aero-mode-leader-def
    :keymaps 'restclient-mode-map
    "RET" '(restclient-http-send-current-stay-in-window :wk "Run query at point")
    "c" '(restclient-http-send-current :wk "Run query at point and focus")
    "r" '(restclient-http-send-current-raw :wk "Run query, no pretty print")
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "." 'restclient-mark-current
    "y" 'restclient-copy-curl-command))

(use-package company-restclient :straight t
  :hook (restclient-mode-hook . company-restclient)
  :after (restclient company)
  :config
  (add-to-list 'company-backends 'company-restclient))


(provide 'aero-web)

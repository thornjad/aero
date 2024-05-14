;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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

(package! jest "thornjad/emacs-jest"
  :commands (jest jest-file jest-test)
  :after (general))

(package! web-mode "fxbois/web-mode"
  :mode "\\.\\(jsp\\|tpl\\|php\\|xml\\|html?\\|erb\\|svg\\|jsx\\|s?css\\)\\'"
  :custom
  (web-mode-enable-engine-detection t))

(package! instant-rename-tag
  (:host github :repo "manateelazycat/instant-rename-tag")
  :after web-mode
  :commands (instant-rename-tag))

(package! emmet-mode :auto
  :hook ((web-mode html-mode css-mode scss-mode js-mode) . emmet-mode)
  :init (setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'js-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))


;; js and jsx

(defun node-repl ()
  "Launch a Node.js comint REPL."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")  ; avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(eval-when-compile (defvar emmet-expand-jsx-className?))
(add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))

(package! json-mode :auto :mode "\\.json\\'")
(package! typescript-mode :auto :mode "\\.ts\\'")
(when (treesitterp) (package! tsx-ts-mode :builtin :mode "\\.tsx\\'"))


;; the rest

(package! restclient :auto
  :after (general)
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'general)
  (aero-mode-leader-def
    :keymaps 'restclient-mode-map
    "RET" '(restclient-http-send-current-stay-in-window :wk "Run query at point")
    "c" '(restclient-http-send-current :wk "Run query at point and focus")
    "r" '(restclient-http-send-current-raw :wk "Run query, no pretty print")
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "." 'restclient-mark-current
    "y" 'restclient-copy-curl-command))


;; yarn.lock derived mode

(defvar yarn-lock-mode-syntax-table
  (let ((syntable (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntable)
    (modify-syntax-entry ?\n ">" syntable)
    (modify-syntax-entry ?\" "\"" syntable)
    syntable))

(defvar yarn-lock-mode-package-re "\\(^\\|,\\s-\\)\\([a-zA-Z-_0-9]+\\)@")
(defvar yarn-lock-mode-dependencies-re "\\s-\\{4,\\}\\([a-zA-Z-_0-9]+\\)\\s-")
(defvar yarn-lock-mode-attributes-re
  (regexp-opt '("version" "resolved" "dependencies" "integrity")))
(defvar yarn-lock-mode-font-lock-defaults
  `((,yarn-lock-mode-attributes-re . '((t :inherit font-lock-builtin-face)))
    (,yarn-lock-mode-package-re . (2 '((t :inherit bold)) t)) ;; Direct deps
    (,yarn-lock-mode-dependencies-re . (1 '((t :inherit bold)) t)) ;; Dep of another dep (nested)
    ))
(define-derived-mode yarn-lock-mode text-mode "Yarn Lock"
  "Simple mode for yarn.lock."
  :syntax-table yarn-lock-mode-syntax-table
  (setq font-lock-defaults '(yarn-lock-mode-font-lock-defaults)
        buffer-read-only t))
(add-to-list 'auto-mode-alist '("yarn\\.lock\\'" . yarn-lock-mode))


(provide 'aero-web)

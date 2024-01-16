;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023-2024 Jade Michael Thornton
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
;;; Commentary:
;;
;;; Code:

;; Automatically install and use treesit grammars
(package! treesit-auto :auto
  :custom
  (treesit-auto-install 'prompt)

  ;; There's a bug right now where it doesn't set its own source list, so we do it here as a
  ;; workaround. Can also add more configuration to this if we want
  (treesit-language-source-alist
   (append (treesit-auto--build-treesit-source-alist)))

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode +1))

;; Tree-sitter-based indentation for select modes
(package! tsi (:host github :repo "orzechowskid/tsi.el")
  :when (treesitterp)
  :hook ((typescript-ts-mode . tsi-typescript-mode)
         (tsx-ts-mode . tsi-typescript-mode)
         (json-ts-mode . tsi-json-mode)
         (css-ts-mode . tsi-css-mode)))

;; Provide vaf, etc. evil selection operators
(package! evil-textobj-tree-sitter
	(:host github :repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries"))
  :when (treesitterp)
	:after (tree-sitter evil)
  :config
  ;; Annoyingly provides no recommended bindings options, so we have to do it ourselves

  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like
  ;; `vif`, `yif`
  (define-key evil-inner-text-objects-map "f"
    (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; Sort of a "dwim", matching the first object found, so vaa, etc.
  (define-key evil-outer-text-objects-map "a"
    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  ;; Goto start of next function
  (define-key evil-normal-state-map
    (kbd "]f") (lambda ()
                 (interactive)
                 (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map
    (kbd "[f") (lambda ()
                 (interactive)
                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map
    (kbd "]F") (lambda ()
                 (interactive)
                 (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map
    (kbd "[F") (lambda ()
                 (interactive)
                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

(package! turbo-log (:host github :repo "Artawower/turbo-log")
  :when (treesitterp)
  :after (general tree-sitter)
  :commands (turbo-log-print
             turbo-log-print-immediately
             turbo-log-comment-all-logs
             turbo-log-uncomment-all-logs
             turbo-log-paste-as-logger
             turbo-log-paste-as-logger-immediately
             turbo-log-delete-all-logs)
  :custom
  (turbo-log-msg-format-template "\"DEBUG LOG: %s\"")
  (turbo-log-allow-insert-without-tree-sitter-p t) ; still works without tree-sitter
  :init
  (aero-leader-def
    "tl" '(:ignore t :wk "turbo-log")
    "tll" 'turbo-log-print
    "tli" 'turbo-log-print-immediately
    "tlh" 'turbo-log-comment-all-logs
    "tls" 'turbo-log-uncomment-all-logs
    "tly" 'turbo-log-paste-as-logger
    "tlY" 'turbo-log-paste-as-logger-immediately
    "tsd" 'turbo-log-delete-all-logs))

(provide 'aero-treesitter)
;;; aero-treesitter.el ends here

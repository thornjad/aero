;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
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

(use-package all-the-icons :straight t)

;; in etc/themes/
(load-theme 'aero-dark t)

(require 'aero-modeline)
(aero/modeline-mode 1)


;;; additional tweaks and packages

;; (when (boundp 'global-prettify-symbols-mode)
;;   (add-hook
;;    'prog-mode-hook
;;    (lambda ()
;;      (setq prettify-symbols-alist
;;            (append prettify-symbols-alist
;;                    '(
;;                      ;; add all greek
;;                      ("lambda" . ?λ)

;;                      ;; mathematics
;;                      ("and" . ?∧)
;;                      ("&&" . ?∧)
;;                      ("or" . ?∨)
;;                      ("||" . ?∨)

;;                      ;; relational algebra
;;                      ("in" . ?∈)
;;                      ("not in" . ?∉)

;;                      ;; arrows and similar
;;                      ("<=" . ?≤)
;;                      (">=" . ?≥)
;;                      ("=>" . ?⇒)
;;                      ("->" . ?→)
;;                      ("!=" . ?≠)
;;                      ("===" . ?≡)
;;                      ("!==" . ?≢)
;;                      ("<<" . ?≪)
;;                      (">>" . ?≫))))))
;;   (global-prettify-symbols-mode t))

(when (window-system)
  (set-frame-font "Victor Mono"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(global-display-fill-column-indicator-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(use-package formfeeder
  :load-path "lib/packages/formfeeder/"
  :hook (text-mode . formfeeder-mode)
  :config
  (setq formfeeder-line-width 80)
  (declare-function global-formfeeder-mode "formfeeder")
  (global-formfeeder-mode 1))

(use-package todo-light
  :load-path "lib/packages/todo-light/"
  :hook ((prog-mode text-mode) . todo-light-mode))

(use-package fireplace :straight t
  :commands fireplace)

;; TODO absorb this into aero
(use-package dashboard :straight t
  :functions (dashboard-modify-heading-icons
              dashboard-setup-startup-hook)
  :defines (dashboard-banner-logo-title
            dashboard-startup-banner
            dashboard-center-content
            dashboard-items
            dashboard-set-heading-icons
            dashboard-set-file-icons
            dashboard-set-init-info)
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title "Aero Emacs"
        dashboard-startup-banner (expand-file-name "logo/aero-logo.png" aero-etc-dir)
        dashboard-center-content t
        dashboard-page-separator "\n\n"
        dashboard-items '((recents  . 5)
                          ;; (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t)
  (dashboard-modify-heading-icons '((recents . "file-text")))
  (add-hook 'dashboard-mode-hook
            (lambda () (display-fill-column-indicator-mode -1)))
  (dashboard-setup-startup-hook))

(provide 'aero-theme)

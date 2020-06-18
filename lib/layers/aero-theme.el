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

;; Other themes to browse from time to time
(use-package doom-themes :straight t
  :defer 10
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package tao-theme :straight t :defer 10
  :config
  (setq tao-theme-use-boxes nil
        tao-theme-use-height nil))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq x-underline-at-descent-line t
      line-spacing 0.1)

;; TODO do we really want this??
(use-package centaur-tabs :straight t :disabled t
  :demand
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'under
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "✧"
        centaur-tabs-cycle-scope 'tabs)

  (define-key evil-normal-state-map (kbd "g t") 'centaur-tabs-forward)
  (define-key evil-normal-state-map (kbd "g T") 'centaur-tabs-backward)
  (aero-leader-def
    "tn" 'centaur-tabs-forward
    "tp" 'centaur-tabs-backward
    "ts" '(centaur-tabs-counsel-switch-group :wk "switch group")
    "tg" '(:ignore t :wk "group tabs by")
    "tgp" '(centaur-tabs-group-by-projectile-project :wk "project")
    "tgn" '(centaur-tabs-group-buffer-groups :wk "normal"))

  (centaur-tabs-group-by-projectile-project)

  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name))))))

  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(require 'aero-modeline)
(aero/modeline-mode 1)


;;; additional tweaks and packages

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
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook #'turn-on-visual-line-mode)
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
  (setq dashboard-banner-logo-title "Aero Emacs"
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

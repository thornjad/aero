;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022, 2024 Jade Michael Thornton
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

(require 'aero-prelude)

(package! aero-modeline :local :load-path "lib/localpackages"
  :config (aero/modeline-global-mode +1))

(package! aero-theme :local :load-path "lib/aero-theme"
  :init
  (when (system-is-mac)
    ;; Default is fine in Linux
    (setq aero-theme-font-height 140))
  :config
  (setq aero-theme-font "JetBrains Mono")
  (load-theme 'aero t))

;; other themes
(package! tao-theme (:host github :repo "11111000000/tao-theme-emacs") :defer t)
(package! spacemacs-theme (:host github :repo "nashamri/spacemacs-theme") :defer t)

(setq default-frame-alist
      ;; width assumes we want a buffer to be 106 columns wide
      (append (list '(width  . 106) '(height . 60)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 0)
                    '(internal-border-width . 8)
                    '(left-fringe . 8) '(right-fringe . 8)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t))))
(if (fboundp 'fringe-mode) (fringe-mode '8))
(set-frame-parameter (selected-frame)
                     'internal-border-width 8)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode +1))

(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places 'right-only
      window-divider-mode t)

;; window margins
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins
             (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;; Better fringe symbols.
(when (and (require 'disp-table nil 'noerror) standard-display-table)
  (set-display-table-slot standard-display-table 'truncation ?…)
  (set-display-table-slot standard-display-table 'wrap ?↩)
  (set-display-table-slot standard-display-table 'selective-display
                          (string-to-vector " …")))

(blink-cursor-mode 0)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(pixel-scroll-mode 1)
(global-hl-line-mode +1)

;; allow navigation by visual wrapped lines rather than real lines
(global-visual-line-mode +1)

;; Adds a breadcrumb to the headerline
(package! breadcrumb
  (:host github :repo "joaotavora/breadcrumb")
  :hook ((prog-mode markdown-mode gfm-mode) . breadcrumb-local-mode)
  :custom (breadcrumb-imenu-max-length 0.98)
  :config
  (with-eval-after-load 'lsp-headerline
    ;; No breadcrumbs when the better lsp-headerline is available
    (add-hook 'lsp-headerline-breadcrumb-mode-hook (lambda () (breadcrumb-local-mode -1))))

  ;; Disable breadcrumb project portion by overriding the function
  (defun breadcrumb-project-crumbs ()
    "Disabled by Aero."
    ""))

;; improved pixel-based smooth scrolling. Mostly used when screen sharing, doesn't really do much
;; for normal navigation
(package! good-scroll (:host github :repo "io12/good-scroll.el")
  :functions (good-scroll-mode)
  :init
  ;; Avoid bug with evil-mode, https://github.com/io12/good-scroll.el/issues/16
  (setq good-scroll-avoid-vscroll-reset nil)
  :config (good-scroll-mode +1))

;; Display formfeed characters
(package! formfeeder (:host gitlab :repo "thornjad/formfeeder" :branch "main")
  :defines (formfeeder-line-width)
  :config
  (setq formfeeder-line-width (- fill-column 1))
  (declare-function global-formfeeder-mode "formfeeder.el")
  (global-formfeeder-mode 1))

;; Highlight the current thing at point, kind of like what lsp-ui does for some language, but in all
;; buffers and modes
(package! highlight-thing (:host github :repo "fgeller/highlight-thing.el")
  :hook (prog-mode . highlight-thing-mode)
  :commands (highlight-thing-mode)
  :custom
  (highlight-thing-delay-seconds 0.5)

  ;; In large buffers, only look in close-by lines
  (highlight-thing-limit-to-region-in-large-buffers-p t)
  (highlight-thing-narrow-region-lines 70)
  (highlight-thing-large-buffer-limit 5000))

;; show all matching selections (from region)
(package! selection-highlight-mode
  (:host github :repo "balloneij/selection-highlight-mode")
  :hook (prog-mode . selection-highlight-mode)
  :custom (selection-highlight-mode-min-length 3))

;; highlight todo and similar words
(package! todo-light (:host gitlab :repo "thornjad/todo-light" :branch "main")
  :hook ((prog-mode text-mode) . todo-light-mode))

;; show indentation guides
(package! highlight-indent-guides
  (:host github :repo "DarthFennec/highlight-indent-guides")
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top)
  :custom
  (highlight-indent-guides-suppress-auto-error t)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (set-face-background 'highlight-indent-guides-top-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-top-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-top-character-face "dimgray"))

;; make links in comments clickable
(global-goto-address-mode +1)

;; Doesn't do anything for GUI, so don't bother. In TUI, use a line when in insert mode
(unless (display-graphic-p)
  (package! evil-terminal-cursor-changer
    (:host github :repo "7696122/evil-terminal-cursor-changer")
    :after evil
    :functions (evil-terminal-cursor-changer-activate)
    :config (evil-terminal-cursor-changer-activate)))

(provide 'aero-ui)

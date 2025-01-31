;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022, 2024, 2025 Jade Michael Thornton
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

(package! aero-modeline :localpackage
  :config
  (aero/modeline-global-mode +1)
  ;; Not using :hook since it's not defined yet for some reason
  (add-hook 'eshell-mode-hook 'aero/modeline-hide-mode))

(package! aero-theme :local :load-path "lib/aero-theme"
  :init
  (when (system-is-mac)
    ;; Default is fine in Linux
    (setq aero-theme-font-height 140))
  :config
  (setq aero-theme-font "JetBrains Mono")
  (load-theme 'aero t))

(setq default-frame-alist
      ;; width assumes we want a buffer to be 106 columns wide
      (append (list '(width  . 212) '(height . 62)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 0)
                    '(internal-border-width . 8)
                    '(left-fringe . 8) '(right-fringe . 8)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t))))
(split-window-horizontally) ; default to two windows
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

(blink-cursor-mode 0)  ; stop cursor from blinking
(show-paren-mode 1)  ; highlight matching parens
(pixel-scroll-mode 1)  ; smoother scrolling

(global-hl-line-mode +1)
(defun aero/suggest-other-faces (func &rest args)
  "When hl-line-mode is active, suggest a face calculated without it.

Credit: Sacha Chua"
  (if global-hl-line-mode
      (progn
        (global-hl-line-mode -1)
        (prog1 (apply func args)
          (global-hl-line-mode 1)))
    (apply func args)))
(advice-add #'face-at-point :around #'aero/suggest-other-faces)

;; allow navigation by visual wrapped lines rather than real lines
(global-visual-line-mode +1)

;; Adds a breadcrumb to the headerline
(package! aero-breadcrumb :localpackage
  :hook ((prog-mode markdown-mode gfm-mode org-mode) . aero-breadcrumb-local-mode)
  :config
  (with-eval-after-load 'gptel
    ;; No breadcrumbs when gptel is running the show
    (add-hook 'gptel-mode-hook (lambda () (aero-breadcrumb-local-mode -1))))
  (with-eval-after-load 'lsp-headerline
    ;; No breadcrumbs when the better lsp-headerline is available
    (add-hook 'lsp-headerline-breadcrumb-mode-hook (lambda () (aero-breadcrumb-local-mode -1)))))

;; improved pixel-based smooth scrolling. Mostly used when screen sharing, doesn't really do much
;; for normal navigation
(package! good-scroll "io12/good-scroll.el"
  :functions (good-scroll-mode)
  :init
  ;; Avoid bug with evil-mode, https://github.com/io12/good-scroll.el/issues/16
  (setq good-scroll-avoid-vscroll-reset nil)
  :config (good-scroll-mode +1))

;; Display formfeed characters
(package! formfeeder "thornjad/formfeeder"
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
(package! todo-light "thornjad/todo-light"
  :init (global-todo-light-mode +1))

;; display time and date in echo area
(package! aero-echo-area :localpackage :defer 4 :config (aero/echo-area-mode +1))

;; Display hex values as their color
(defun aero/rainbow-mode ()
  "Display colors represented as hex values."
  (interactive)
  (remove-overlays (point-min) (point-max))
  (let ((hex-color-regex "#[0-9a-fA-F]\\{3,6\\}"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward hex-color-regex nil t)
        (let* ((color (match-string 0))
               (overlay (make-overlay (match-beginning 0) (match-end 0))))
          (if (string-greaterp color "#888888")
              (overlay-put overlay 'face `(:background ,color :foreground "black"))
            (overlay-put overlay 'face `(:background ,color :foreground "white"))))))))
(defun aero/rainbow-mode-disable ()
  "Remove all hex color overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max)))
(add-hook 'prog-mode-hook #'aero/rainbow-mode)
(add-hook 'org-mode-hook #'aero/rainbow-mode)
(add-hook 'conf-space-mode-hook #'aero/rainbow-mode)

;; make links in comments clickable
(global-goto-address-mode +1)

;; Doesn't do anything for GUI, so don't bother. In TUI, use a line when in insert mode
(unless (display-graphic-p)
  (package! evil-terminal-cursor-changer
    (:host github :repo "7696122/evil-terminal-cursor-changer")
    :after evil
    :functions (evil-terminal-cursor-changer-activate)
    :config (evil-terminal-cursor-changer-activate)))

;; Pulse the current line when changing windows
(dolist (fn '(other-window
              windmove-up
              windmove-down
              windmove-left
              windmove-right
              aero/alternate-buffer
              aero/alternate-window))
  (advice-add fn :after #'pulse-line))

(provide 'aero-ui)

;;; aero-ui.el ends here

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton
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


(require 'aero-modeline)
(aero/modeline-global-mode +1)

(package! aero-theme :local
  :load-path "lib/packages/aero-theme/"
  :init
  (when (system-is-mac)
    (setq aero-theme-font-height 140))
  :config
  (setq aero-theme-font "JetBrains Mono")
  (load-theme 'aero t))

;; other themes
(package! tao-theme :auto :defer t)
(package! spacemacs-theme :auto :defer t)

(setq default-frame-alist
      ;; width assumes we want a buffer to be 106 columns wide
      (append (list '(width  . 106) '(height . 60)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 0)
                    '(internal-border-width . 8)
                    '(left-fringe . 8) '(right-fringe . 8)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t)
                    '(ns-appearance . (if (display-grapic-p) light dark)))))
(if (fboundp 'fringe-mode) (fringe-mode '8))
(set-frame-parameter (selected-frame)
                     'internal-border-width 8)
(when (fboundp 'pixel-scroll-precision-mode)
	(pixel-scroll-precision-mode +1))
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)


(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places 'right-only
      window-divider-mode t)

;; Initial window split in half
;; (split-window-horizontally)

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


;;; additional tweaks and packages

(blink-cursor-mode 0)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(pixel-scroll-mode 1)
(global-visual-line-mode +1) ; allow navigation by visual wrapped lines rather than real lines

(global-hl-line-mode +1)

(package! good-scroll (:host github :repo "io12/good-scroll.el")
  :functions (good-scroll-mode)
  :init
  ;; Avoid bug with evil-mode, https://github.com/io12/good-scroll.el/issues/16
  (setq good-scroll-avoid-vscroll-reset nil)
  :config (good-scroll-mode +1))

(package! formfeeder (:host gitlab :repo "thornjad/formfeeder" :branch "main")
  :defines (formfeeder-line-width)
  :config
  (setq formfeeder-line-width (- fill-column 1))
  (declare-function global-formfeeder-mode "formfeeder.el")
  (global-formfeeder-mode 1))

(package! todo-light (:host gitlab :repo "thornjad/todo-light" :branch "main")
  :hook ((prog-mode text-mode) . todo-light-mode))

(package! highlight-indent-guides :auto
  :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (set-face-background 'highlight-indent-guides-top-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-top-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-top-character-face "dimgray"))

(package! echo-bar (:host github :repo "qaiviq/echo-bar.el")
  :defer 2
  :config
  (defun aero/echo-bar-function ()
    (concat
     (format-time-string "[ %R | %A, %d %b")
     (when-let ((bat (ignore-errors (funcall battery-status-function))))
       (concat " | " (cdr (cadr bat)) "%"))
     " ]"))
  (setq echo-bar-function #'aero/echo-bar-function)
  (echo-bar-mode +1))

;; make links in comments clickable
(global-goto-address-mode +1)

(provide 'aero-ui)
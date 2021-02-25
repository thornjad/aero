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

(require 'use-package)

;;; Code:

(require 'aero-modeline)
(aero/modeline-global-mode +1)

;; default themes
;; TODO this causes all frames to change, we only want frame-local if possible
(defun aero/load-default-theme (&optional frame)
  "Set FRAME-local theme."
  (with-selected-frame (or frame (selected-frame))
    (if (display-graphic-p)
        (load-theme 'aero-light t)
      (load-theme 'aero-dark t))))
;; Run immediately for the new frame
(aero/load-default-theme)
;; Set up for emacsclient
(add-hook 'after-make-frame-functions #'aero/load-default-theme)


;; Other themes to browse from time to time
;; (use-package doom-themes :straight t
;;   :defer 10
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (doom-themes-neotree-config)
;;   (doom-themes-org-config))
;; (use-package poet-theme :straight t :defer 10)
;; (use-package spacemacs-theme :straight t :defer 10)
;; (use-package spaceline :straight t)
;; (use-package tao-theme :straight t :defer 10
;;   :config
;;   (setq tao-theme-use-boxes nil
;;         tao-theme-use-height nil))

;; Start the initial frame maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Subsequent frames will be smaller
(setq default-frame-alist
      (append (list '(width  . 200) '(height . 60)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 0)
                    '(internal-border-width . 20)
                    '(left-fringe . 0) '(right-fringe . 0)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t)
                    '(ns-appearance . dark))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 20)


;;; get ligatures to actually work

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

;; Better fringe wrap symbols. Victor Mono doesn't provide these symbols, so use
;; Fira Code
(fringe-mode '(0 . 0))
(defface fallback
  '((t :family "Fira Code Light" :inherit 'default))
  "Fallback font"
  :group 'faces)
(when (and (require 'disp-table nil 'noerror) standard-display-table)
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback))
  (set-display-table-slot standard-display-table 'selective-display
                          (string-to-vector " …")))


;;; additional tweaks and packages

(blink-cursor-mode 0)
(tool-bar-mode 0)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(pixel-scroll-mode 1)
(global-display-fill-column-indicator-mode 1)
(global-visual-line-mode +1)

(use-package good-scroll
  :straight (:host github :repo "io12/good-scroll.el")
  :config (good-scroll-mode +1))

(use-package all-the-icons :straight t)

;; TODO doesn't seem to be working?
(use-package formfeeder :straight (:host gitlab :repo "thornjad/formfeeder")
  :config
  (setq formfeeder-line-width 80)
  (declare-function global-formfeeder-mode "formfeeder")
  (global-formfeeder-mode 1))

(use-package todo-light
  :straight (:host gitlab :repo "thornjad/todo-light")
  :hook ((prog-mode text-mode) . todo-light-mode))

(use-package fireplace :straight t
  :commands fireplace)

;; TODO zone-when-idle??

(defun pulse-line (&rest _)
  "Briefly pulse a highlight of the line at point.
This function, when bound to certain commands like scrolling, acts as a native
alternative to the beacon package."
  (pulse-momentary-highlight-one-line (point)))
(dolist (cmd '(scroll-up-command
               scroll-down-command
               recenter-top-bottom
               other-window))
  (advice-add cmd :after #'pulse-line))
(when (require 'evil nil 'no-error)
  (dolist (cmd '(evil-goto-first-line
                 evil-goto-line
                 evil-scroll-up
                 evil-scroll-down
                 evil-scroll-line-to-center
                 evil-scroll-line-to-top
                 evil-scroll-line-to-bottom
                 evil-yank
                 evil-yank-line
                 evil-yank-rectangle
                 evil-yank-characters
                 evil-window-top
                 evil-window-middle
                 evil-window-bottom))
    (advice-add cmd :after #'pulse-line)))

(provide 'aero-theme)

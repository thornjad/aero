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

(require 'aero-modeline)
(aero/modeline-global-mode +1)

;; in etc/themes/
(load-theme 'aero-light t)

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
(use-package tao-theme :straight t :defer 10
  :config
  (setq tao-theme-use-boxes nil
        tao-theme-use-height nil))

(setq default-frame-alist
      (append (list '(width  . 120) '(height . 45)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 1)
                    '(internal-border-width . 10)
                    '(left-fringe . 0) '(right-fringe . 0)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t)
                    '(ns-appearance . dark))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 10)

(setq-default x-underline-at-descent-line t
              line-spacing 0.1
              widget-image-enable nil)

(use-package svg-tag-mode
  :straight (:host github :repo "rougier/svg-tag-mode" :branch "main")
  :defer 3
  :config
  (defface svg-tag-note-face
    '((t :foreground "black" :background "white" :box "black"
         :family "Victor Mono" :weight light :height 120))
    "Face for note tag" :group nil)

  (defface svg-tag-keyboard-face
    '((t :foreground "#333333" :background "#f9f9f9" :box "#333333"
         :family "Victor Mono" :weight light :height 120))
    "Face for keyboard bindings tag" :group nil)

  (setq svg-tag-todo (svg-tag-make "TODO" nil 1 1 3))
  (setq svg-tag-fixme (svg-tag-make "FIXME" nil 1 1 3))
  (setq svg-tag-note (svg-tag-make "NOTE" 'svg-tag-note-face 1 1 3))
  (setq svg-tag-hack (svg-tag-make "HACK" 'svg-tag-note-face 1 1 3))

  (defun svg-tag-round (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 12))
  (defun svg-tag-quasi-round (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 8))
  (defun svg-tag-keyboard (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-keyboard-face 1 1 2))

  (setq svg-tag-tags
        '(("TODO"                     . svg-tag-todo)
          ("FIXME"                    . svg-tag-fixme)
          ("NOTE"                     . svg-tag-note)
          ("HACK"                     . svg-tag-hack)
          ("\([0-9a-zA-Z]\)"            . svg-tag-round)
          ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . svg-tag-quasi-round)
          ("|[0-9a-zA-Z- ]+?|"          . svg-tag-keyboard)))
  (svg-tag-mode 1))

;; date in echo area
(require 'subr-x)

;; Improved version of enhanced-message.el
;; https://gist.github.com/rougier/baaf4ff6e0461680e3f070c5c32b64a2
(defun enhanced-message (orig-fun &rest args)
  "Message ORIG-FUN with ARGS, but add additional text.
This enhanced message displays a regular message in the echo area and adds a
specific text on the right part of the echo area. This is to be used as an
advice."
  (let* ((right
          (propertize
           ;; HACK The first space is a thin space, not a regular space. We'll
           ;; split on the thin space later so-as not to inadvertently break up
           ;; a real message
           (format-time-string "   %A %d %B %Y, %H:%M  ")
           'face '(:height 0.85
                   :overline t
                   :family "Futura"
                   :inherit mode-line-inactive)))
         (width (- (frame-width) (length right) -4))
         (msg (if (car args) (apply 'format-message args) ""))
         (msg (car (split-string msg " ")))
         (msg (string-trim msg))
         (left (truncate-string-to-width msg width nil nil "…"))
         (full (format (format "%%-%ds %%s" width) left right)))
    (if (active-minibuffer-window)
        ;; Regular log and display when minibuffer is active
        (apply orig-fun args)
      (progn
        ;; Log actual message without echo
        (if message-log-max
            (let ((inhibit-message t)) (apply orig-fun (list msg))))
        ;; Display enhanced message without log
        (let ((message-truncate-lines t) (message-log-max nil))
          (apply orig-fun (list full)))
        ;; Set current message explicitely
        (setq current-message msg)))))

(advice-add 'message :around #'enhanced-message)
(add-hook 'post-command-hook
          (lambda () (let ((message-log-max nil))
                       (message (current-message)))))


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
(defface fallback '((t :family "Fira Code Light"
                       :inherit 'default)) "Fallback")
;; TODO these aren't working, they say "wrong-type-argument char-table-p nil" and show that standard-display-table is nil? Do we need to wait for somethign to eval?
;(set-display-table-slot standard-display-table 'truncation
;                        (make-glyph-code ?… 'fallback))
;(set-display-table-slot standard-display-table 'wrap
;                        (make-glyph-code ?↩ 'fallback))
;(set-display-table-slot standard-display-table 'selective-display
;                        (string-to-vector " …"))


;;; additional tweaks and packages

(blink-cursor-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(pixel-scroll-mode 1)
(global-display-fill-column-indicator-mode 1)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook #'turn-on-visual-line-mode)

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

(provide 'aero-theme)

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
(require 'use-package)


(require 'aero-modeline)
(aero/modeline-global-mode +1)

(use-package aero-theme :straight nil
  :load-path "lib/packages/aero-theme/"
  :init
  (when (system-is-mac)
    (setq aero-theme-font-height 140))
  :config
  (setq aero-theme-font "IBM Plex Mono")
  (load-theme 'aero t))

;; other themes
(use-package tao-theme :straight t :defer t)
(use-package spacemacs-theme :straight t :defer t)

(setq default-frame-alist
      (append (list '(width  . 212) '(height . 60)
                    '(tool-bar-lines . 0)
                    '(menu-bar-lines . 0)
                    '(internal-border-width . 6)
                    '(left-fringe . 8) '(right-fringe . 8)
                    '(vertical-scroll-bars . nil)
                    '(ns-transparent-titlebar . t)
                    '(ns-appearance . (if (display-grapic-p) light dark)))))
(if (fboundp 'fringe-mode) (fringe-mode '8))
(set-frame-parameter (selected-frame)
                     'internal-border-width 6)

;; window margins
(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-margins
             (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

;; set up initial window the way i want it
(split-window-horizontally)

;; not working:
;; (aero/frame-recenter)
;; (add-hook 'after-make-frame-functions #'aero/frame-recenter)

;; Startup with vterm running
(when (require 'vterm-toggle nil t)
  (vterm-toggle))


;; get ligatures to actually work

;; Combination of ligature list from https://github.com/robbert-vdh/dotfiles and my own
;; experimentation
(let ((alist '((119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
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
               (?! . "\\(?:!\\(?:==\\|[!=]\\)\\)")
               (?# . "\\(?:#\\(?:###?\\|_(\\|[!#(:=?[_{]\\)\\)")
               (?$ . "\\(?:\\$>\\)")
               (?& . "\\(?:&&&?\\)")
               (?* . "\\(?:\\*\\(?:\\*\\*\\|[/>]\\)\\)")
               (?+ . "\\(?:\\+\\(?:\\+\\+\\|[+>]\\)\\)")
               (?- . "\\(?:-\\(?:-[>-]\\|<<\\|>>\\|[<>|~-]\\)\\)")
               (?. . "\\(?:\\.\\(?:\\.[.<]\\|[.=?-]\\)\\)")
               (?/ . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")
               (?: . "\\(?::\\(?:::\\|\\?>\\|[:<-?]\\)\\)")
               (?< . "\\(?:<\\(?:!--\\|\\$>\\|\\*>\\|\\+>\\|-[<>|]\\|/>\\|<[<=-]\\|=\\(?:=>\\|[<=>|]\\)\\||\\(?:||::=\\|[>|]\\)\\|~[>~]\\|[$*+/:<=>|~-]\\)\\)")
               (?= . "\\(?:=\\(?:!=\\|/=\\|:=\\|=[=>]\\|>>\\|[=>]\\)\\)")
               (?> . "\\(?:>\\(?:=>\\|>[=>-]\\|[]:=-]\\)\\)")
               (?? . "\\(?:\\?[.:=?]\\)")
               (?\ . "\\(?:\\\\/?\\)")
               (?\; . "\\(?:;;\\)")
               (?\[ . "\\(?:\\[\\(?:||]\\|[<|]\\)\\)")
               (?\] . "\\(?:]#\\)")
               (?^ . "\\(?:\\^=\\)")
               (?_ . "\\(?:_\\(?:|?_\\)\\)")
               (?{ . "\\(?:{|\\)")
               (?| . "\\(?:|\\(?:->\\|=>\\||\\(?:|>\\|[=>-]\\)\\|[]=>|}-]\\)\\)")
               (?~ . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

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

;; shiny new native fill-line indicator
;; TODO this is not using the character I'm setting here?
(setq display-fill-column-indicator-character ?│)
(global-display-fill-column-indicator-mode 1)
(global-hl-line-mode +1)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(use-package good-scroll
  :straight (:host github :repo "io12/good-scroll.el")
  :functions (good-scroll-mode)
  :init
  ;; Avoid bug with evil-mode, https://github.com/io12/good-scroll.el/issues/16
  (setq good-scroll-avoid-vscroll-reset nil)
  :config (good-scroll-mode +1))

(use-package formfeeder
  :straight (:host gitlab :repo "thornjad/formfeeder" :branch "main")
  :defines (formfeeder-line-width)
  :config
  (setq formfeeder-line-width (- fill-column 1))
  (declare-function global-formfeeder-mode "formfeeder.el")
  (global-formfeeder-mode 1))

(use-package todo-light
  :straight (:host gitlab :repo "thornjad/todo-light" :branch "main")
  :hook ((prog-mode text-mode) . todo-light-mode))

(use-package highlight-indent-guides
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


;; make links in comments clickable
(global-goto-address-mode +1)

(provide 'aero-ui)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
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


(setq-default
 ;; general
 ring-bell-function 'ignore ; supprime cette putain de cloche.
 sentence-end-double-space nil ; the world will not go to shit today
 default-fill-column 80 ; i am mortal, not arthur whitney
 fill-column 80 ; same, bro
 help-window-select t ; focus help window when opened
 kill-ring-max 5000 ; truncate kill ring after 5000 entries
 mark-ring-max 5000 ; truncate mark ring after 5000 entries
 kill-do-not-save-duplicates t
 apropos-do-all t ; apropos is apropos
 global-display-line-numbers-mode nil ; fuck line numbers
 gnutls-min-prime-bits 4096 ; 256 est absurde
 confirm-kill-emacs 'yes-or-no-p ; too easy to kill when looking for alternate file
 line-move-visual t ; move lines by display, not reality
 make-pointer-invisible t ; le curseur est une chienne
 auto-revert-interval 10 ; wait just a little longer (default is 5)
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; scroll one line at a time
 pixel-resolution-fine-flag 1
 frame-title-format `("Aero Emacs")

 ;; startup with scratch
 inhibit-startup-screen t
 inhibit-splash-screen t
 initial-buffer-choice t
 inhibit-startup-echo-area-message t
 initial-buffer-choice (lambda ()
                         (unless (get-buffer "*dashboard*")
                           (get-buffer "*scratch*")))
 initial-major-mode 'text-mode
 initial-scratch-message ";; Aero Emacs\n\n"


 ;; version control and saving
 use-package-verbose nil
 delete-old-versions -1 ; supprime les vieilles versions des fichiers sauvegardés
 backup-directory-alist `(("." . "~/.config/emacs/backups"))
 version-control t
 vc-follow-symlinks t
 git-commit-fill-column 72
 auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t))
 save-interprogram-paste-before-kill t
 diff-switches "-u" ; unified diff by default

 ;; files
 confirm-nonexistent-file-or-buffer nil ; don't ask to create a buffer
 require-final-newline t
 load-prefer-newer t
 read-file-name-completion-ignore-case t ; ignorer la capitalisation des fichiers
 delete-auto-save-files t ; auto-delete auto-save auto-files automatically

 org-agenda-files (file-expand-wildcards "~/doc/org/*.org")

 ;; indentation
 indent-tabs-mode t
 tab-width 2                ; onglet affiché sous forme de 2
 c-basic-offset 2
 sh-basic-offset 2
 cperl-indent-level 2
 js2-basic-offset 2
 js-basic-offset 2
 js-indent-level 2
 js-switch-indent-offset 2
 js-syntactic-mode-name nil ; just use normal mode name
 sgml-basic-offset 2
 tcl-indent-level 2
 tcl-tab-always-indent t
 css-indent-offset 2
 rust-indent-offset 2
 evil-shift-width 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2
 tab-stop-list (number-sequence 2 200 2)
 auto-window-vscroll nil)

;; ensure lang is set properly
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; remplace yes no par y n
(defalias 'yes-or-no-p 'y-or-n-p)

;; make case statements indent properly
(c-set-offset 'case-label '++)

;; prevent savehist cpu hogging
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Don't kill my scratch!"
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Trigger auto-fill after punctutation characters, not just whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; type to get rid of active selection
(delete-selection-mode t)

(blink-cursor-mode 0)

(pixel-scroll-mode 1)

(when (string= system-type "darwin")
	(setq-default dired-use-ls-dired nil))

;; ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun display-startup-echo-area-message ()
  "Override ridiculous built-in crap."
  (message "Aero est prêt"))

;; If there were no compilation errors, delete the compilation window
(setq-default compilation-exit-message-function
              (lambda (status code msg)
                ;; If M-x compile exists with a 0
                (when (and (eq status 'exit) (zerop code))
                  ;; then bury the *compilation* buffer, so that C-x b doesn't go there
                  (bury-buffer "*compilation*")
                  ;; and return to whatever were looking at before
                  (replace-buffer-in-windows "*compilation*"))
                ;; Always return the anticipated result of compilation-exit-message-function
                (cons msg code)))

;; open some buffers in the same window
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window)
             '("*helpful*" display-buffer-same-window))

(provide 'aero-rc)

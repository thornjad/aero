;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs
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
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ; make mouse scrolling smooth
 apropos-do-all t ; apropos is apropos
 global-display-line-numbers-mode nil ; fuck line numbers
 gnutls-min-prime-bits 4096 ; 256 est absurde
 confirm-kill-emacs 'yes-or-no-p ; too easy to kill when looking for alternate file
 line-move-visual t ; move lines by display, not reality
 make-pointer-invisible t ; le curseur est une chienne
 auto-revert-interval 10 ; wait just a little longer (default is 5)

 ;; startup with scratch
 inhibit-startup-screen t
 inhibit-splash-screen t
 initial-buffer-choice t
 inhibit-startup-echo-area-message t
 initial-buffer-choice (lambda () (get-buffer "*scratch*"))
 initial-major-mode 'emacs-lisp-mode
 initial-scratch-message ";; Aero Emacs\n\n"

 ;; version control and saving
 use-package-verbose nil
 delete-old-versions -1	; supprime les vieilles versions des fichiers sauvegardés
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 version-control t
 vc-follow-symlinks t
 git-commit-fill-column 72
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
 save-interprogram-paste-before-kill t
 diff-switches "-u" ; unified diff by default

 ;; files
 confirm-nonexistent-file-or-buffer nil ; don't ask to create a buffer
 recentf-max-saved-items 5000           ; save up to 5000 recent files
 require-final-newline t
 load-prefer-newer t
 scroll-margin 3
 read-file-name-completion-ignore-case t ; ignorer la capitalisation des fichiers
 delete-auto-save-files t ; auto-delete auto-save auto-files automatically

 ;; indentation
 indent-tabs-mode t
 tab-width 2                ; onglet affiché sous forme de 2
 c-basic-offset 2
 cperl-indent-level 2
 js2-basic-offset 2
 js-basic-offset 2
 sgml-basic-offset 2
 tcl-indent-level 2
 tcl-tab-always-indent t
 css-indent-offset 2
 rust-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2
 tab-stop-list (number-sequence 2 200 2)
 auto-window-vscroll nil)

;; ensure lang is set properly
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; store all backup and autosave files in the tmp dir
(setq-default backup-directory-alist `((".*" . ,temporary-file-directory))
							auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; remplace yes no par y n
(defalias 'yes-or-no-p 'y-or-n-p)

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

;; display changes
(aero/add-hook! 'before-save-hook
	(delete-trailing-whitespace))

;; type to get rid of active selection
(delete-selection-mode t)

(when (string= system-type "darwin")
	(setq-default dired-use-ls-dired nil))

;; rend les scripts executable par défault si c'est un script.
(aero/add-hook! 'after-save-hook
	(executable-make-buffer-file-executable-if-script-p))

;; ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun display-startup-echo-area-message ()
  "Override ridiculous built-in crap."
  (message "Aero est prêt"))

(provide 'aero-rc)

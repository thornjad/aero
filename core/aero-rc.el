;; -*- lexical-binding: t -*-
;; Aero main configuration
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


;; global defaults

(setq-default

 ;; general
 initial-scratch-message "Welcome to Aero"
 ring-bell-function 'ignore ; supprime cette putain de cloche.
 sentence-end-double-space nil
 default-fill-column 80
 help-window-select t ; focus help window when opened
 kill-ring-max 5000 ; truncate kill ring after 5000 entries
 mark-ring-max 5000 ; truncate mark ring after 5000 entries
 mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))) ;make mouse scrolling smooth
 apropos-do-all t
 mouse-yank-at-point t
 global-display-line-numbers-mode nil ; fuck line numbers

 ;; version control and saving
 use-package-verbose nil
 delete-old-versions -1	    ; supprime les vieilles versions des fichiers sauvegardés
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 version-control t
 vc-make-backup-files t	    ; backups file even when under vc
 vc-follow-symlinks t
 git-commit-fill-column 72
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
 save-interprogram-paste-before-kill t

 ;; files
 confirm-nonexistent-file-or-buffer nil ;don't ask to create a buffer
 recentf-max-saved-items 5000           ;save up to 5000 recent files
 require-final-newline t
 load-prefer-newer t
 scroll-margin 3

 ;; indentation
 indent-tabs-mode t
 tab-stop-list (number-sequence 2 200 2)
 tab-width 2                ; onglet affiché sous forme de 2
 c-basic-offset 2
 standard-indent 2
 auto-window-vscroll nil)

;; ensure lang is set properly
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; store all backup and autosave files in the tmp dir
(setq-default backup-directory-alist `((".*" . ,temporary-file-directory))
							auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; remplace yes no par y n
(defalias 'yes-or-no-p 'y-or-n-p)


;; modes

;; display changes
(show-paren-mode 1) ; highlight delimiters
(line-number-mode -1) ; only show line number in mode line
(column-number-mode -1) ; also show column in mode line
(setq-default initial-major-mode 'fundamental-mode)
(aero/add-hook! 'before-save-hook
	(delete-trailing-whitespace))
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

;; type to get rid of active selection
(delete-selection-mode t)


;; system-specific

(when (string= system-type "darwin")
	(setq-default dired-use-ls-dired nil))
(unless (eq window-system 'ns)
  (menu-bar-mode -1))


;; et cetera

;; rend les scripts executable par défault si c'est un script.
(aero/add-hook! 'after-save-hook
	(executable-make-buffer-file-executable-if-script-p))

;; turn off unneeded stuff when unneeded
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defvar aero/default-font "Dank Mono"
	"Default font throughout Aero")

(global-prettify-symbols-mode t)

(defvar aero/gc-cons '(16777216 0.1)) ; 16 mB

(provide 'aero-rc)

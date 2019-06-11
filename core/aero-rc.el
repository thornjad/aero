;; -*- lexical-binding: t -*-
;; Aero main configuration
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; permission to use, copy, modify, and/or
;; distribute this software for any purpose with or without fee is hereby
;; granted, provided that the above copyright notice and this permission notice
;; appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, Negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
;;
;; This file is not part of GNU Emacs

(setq
 use-package-verbose nil
 delete-old-versions -1	    ; supprime les vieilles versions des fichiers sauvegardés
 backup-directory-alist `(("." . "~/.emacs.d/backups"))
 version-control t
 vc-make-backup-files t	    ; backups file even when under vc
 vc-follow-symlinks t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
 initial-scratch-message "Welcome to Aero"
 ring-bell-function 'ignore ; supprime cette putain de cloche.
 sentence-end-double-space nil
 default-fill-column 80
 initial-scratch-message ""
 save-interprogram-paste-before-kill t
 help-window-select t       ; focus help window when opened
 tab-width 2                ; onglet affiché sous forme de 2
 auto-window-vscroll nil
 )
(setq-default
 indent-tabs-mode t
 tab-width 2
 c-basic-offset 2)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; remplace yes no par y n
(defalias 'yes-or-no-p 'y-or-n-p)

;; display changes
(show-paren-mode) ; highlight delimiters
(line-number-mode -1) ; only show line number in mode line
(column-number-mode -1) ; also show column in mode line
(setq initial-major-mode 'fundamental-mode)
(aero/add-hook! 'before-save-hook
	(delete-trailing-whitespace))

;; rend les scripts executable par défault si c'est un script.
(aero/add-hook! 'after-save-hook
	(executable-make-buffer-file-executable-if-script-p))

(defvar aero/default-font "Dank Mono"
	"Default font throughout Aero")

(set-face-attribute 'default nil :font aero/default-font)

(defvar aero/gc-cons '(100000000 0.1))

(provide 'aero-rc)

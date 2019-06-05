;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;; This file is not part of GNU Emacs

;; Avoid garbage collection during startup.
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184
			gc-cons-percentage 0.6)

;;; Prepare and bootstrap `use-package'
(setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
	(normal-top-level-add-subdirs-to-load-path))
(package-initialize t)
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
(eval-when-compile
	(require 'use-package))
(use-package package
	:config
	(setq package-check-signature nil)
	(setq package-enable-at-startup nil)
	(setq package-archives '(("melpa" . "https://melpa.org/packages/")
													 ("marmalade" . "https://marmalade-repo.org/packages/")
													 ("org" . "https://orgmode.org/elpa/")
													 ("gnu" . "https://elpa.gnu.org/packages/")
													 ("elpy" . "https://jorgenschaefer.github.io/packages/"))))

;; Define load-paths
(load (concat (file-name-directory load-file-name)
							"core/load-paths.el"))

;; disable file-name-handlers for a speed boost during startup
(let ((file-name-handler-alist nil))
	(require 'core-aero))

;;; Defaults
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
(prefer-coding-system 'utf-8)

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
(add-hook! 'before-save-hook
					 (delete-trailing-whitespace))

;; rend les scripts executable par défault si c'est un script.
(add-hook! 'after-save-hook
					 (executable-make-buffer-file-executable-if-script-p))

(defvar aero/default-font "Dank Mono"
	"Default font throughout Aero")

(set-face-attribute 'default nil :font aero/default-font)

;; TODO set gc-cons-threshold 100000000
;; TODO set gc-cons-percentage 0.1

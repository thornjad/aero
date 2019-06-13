;; -*- lexical-binding: t -*-
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

;; Avoid garbage collection during startup.
(setq gc-cons-threshold 402653184
			gc-cons-percentage 0.6)

;; vérifier les erreurs dans ce fichier
(setq debug-on-error t)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))


;; Set up load paths

(defun add-to-load-path (dir)
	(add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
	(when (file-exists-p dir)
		(add-to-load-path dir)))

(defmacro def-path! (name base dir)
	"Define a directory constant in the `dir' directory of `base'"
	(let ((dir-name (intern (concat "aero-" (symbol-name name) "-directory")))
				(dir-base (intern (concat "aero-" (symbol-name base) "-directory"))))
		`(defconst ,dir-name
			 (expand-file-name (concat ,dir-base ,dir)))))

(setq user-init-file
			(or load-file-name (buffer-file-name)))
(setq user-emacs-directory
			(file-name-directory user-init-file))
(defvar aero-start-directory
	user-emacs-directory)

(def-path! core start "core/")
(def-path! layer start "layers/")
(def-path! private start "private/")
(def-path! cache start ".cache/")
(def-path! autosave cache "auto-save/")
(def-path! test start "test/")

(defconst user-home-directory
	(getenv "HOME"))
(defconst pcache-directory
	(concat aero-cache-directory "pcache/"))
(unless (file-exists-p aero-cache-directory)
	(make-directory aero-cache-directory))

(mapc 'add-to-load-path
			`(,aero-core-directory
				,aero-layer-directory
				,(concat aero-core-directory "libs/")
				,(concat aero-core-directory "libs/aero-theme/")))

(add-to-list 'custom-theme-load-path
						 (concat aero-core-directory
										 "libs/aero-theme/"))


;; Core functionality

(defun aero/bootstrap-package ()
	"Bootstrap `use-package' and set up for use"
	(setq package-enable-at-startup nil)
	(let ((default-directory "~/.emacs.d/elpa"))
		(normal-top-level-add-subdirs-to-load-path))
	(setq package-archives '(("melpa" . "https://melpa.org/packages/")
													 ("org" . "https://orgmode.org/elpa/")
													 ("gnu" . "https://elpa.gnu.org/packages/")
													 ("elpy" . "https://jorgenschaefer.github.io/packages/")))
	(package-initialize)
	(unless (package-installed-p 'use-package)
		(package-refresh-contents)
		(package-install 'use-package))
	(eval-when-compile
		(require 'use-package))
	(use-package package
		:config (setq package-check-signature nil)))

(defgroup aero nil
	"Aero customizations"
	:group 'starter-kit
	:prefix 'aero/)

(defun aero/startup-echo-message ()
	(message "Aero is ready"))

(defvar aero-initialized nil
	"Whether Aero has finished initialization")

(defun aero/init ()
	"Perform startup initialization"

	;; silence ad-handle-definition without advised functions being redefined
	(setq ad-redefinition-action 'accept)
	;; explicitly set utf-8 to avoid prompt from emacs
	(prefer-coding-system 'utf-8)

	;; TODO
	;; (setq-default evil-want-C-u-scroll t
  ;;               ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
  ;;               ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
  ;;               ;; instead.
  ;;               evil-want-C-i-jump nil)

	(require 'aero-layers)
	(aero/load-layers))

(defun aero/startup-hook ()
	"Post-init processing"
	(add-hook
	 'emacs-startup-hook
	 (defun aero/init-hook ()
		 (require 'aero-rc)
		 (setq aero-initialized t)
		 (setq gc-cons-threshold (car aero/gc-cons)
					 gc-cons-percentage (cadr aero/gc-cons))
		 (global-font-lock-mode))))


;; The actual initilization

;; disable file-name-handlers for a speed boost during startup
(let ((file-name-handler-alist nil))
	(aero/bootstrap-package)

	(require 'subr-x)
	(require 'aero-util)

	;; burn baby burn
	(aero/init)
	(aero/startup-hook)

	;; TODO
	;; (global-undo-tree-mode t)
	;; (winner-mode t)

	;; safe, no more debug please
	(setq debug-on-error nil)
	(aero/startup-echo-message))

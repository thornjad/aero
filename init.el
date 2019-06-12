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
				,(concat aero-core-directory "libs/")
				,(concat aero-core-directory "libs/aero-theme/")))

(add-to-list 'custom-theme-load-path
						 (concat aero-core-directory
										 "libs/aero-theme/"))


;; Bootstrap `use-package'

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
	:config (setq package-check-signature nil))


;; Core functionality

(defun aero/init-core-keybindings ()
	(general-define-key
	 :states '(normal visual insert emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 ;; simple commands
	 "TAB" '(switch-to-other-buffer :which-key "prev buffer")
	 "SPC" '(counsel-M-x :which-key "M-x")
	 "'" '(eshell :which-key "eshell")))

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
	(setq-default evil-want-C-u-scroll t
                ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
                ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
                ;; instead.
                evil-want-C-i-jump nil)

	(use-package counsel :ensure t
		:config
		(setq counsel-find-file-ignore-regexp
					(concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
									"\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
									"\\|\\.aux\\'")))

	(aero/load-layers)
	(aero/load-theme)
	(aero/init-core-keybindings))

(defun aero/startup-hook ()
	"Post-init processing"
	(add-hook
	 'emacs-startup-hook
	 (defun aero/init-hook ()
		 (require 'aero-rc)
		 (setq aero-initialized t)
		 (setq gc-cons-threshold (car aero/gc-cons)
					 gc-cons-percentage (cadr aero/gc-cons)))))


;; The actual initilization

;; disable file-name-handlers for a speed boost during startup
(let ((file-name-handler-alist nil))
	(require 'subr-x)
	(require 'aero-util)
	(require 'aero-layers)
	(require 'aero-theme)

	;; garder ma merde à jour
	(use-package auto-update-package
		:ensure t
		:config
		(setq auto-package-update-delete-old-versions t
					auto-package-update-hide-results nil)
		(auto-package-update-maybe))

	;; Set up global functionality
	(use-package which-key :ensure t)
	(use-package general :ensure t)
	(use-package diminish :ensure t)

	;; burn baby burn
	(aero/init)
	(aero/startup-hook)
	(global-font-lock-mode)

	;; TODO
	;; (global-undo-tree-mode t)
	;; (winner-mode t)

	;; safe, no more debug please
	(setq debug-on-error nil)
	(aero/startup-echo-message))

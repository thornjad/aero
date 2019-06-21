;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2017-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs


;;; Set up load paths

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
(def-path! cache start ".cache/")
(def-path! autosave cache "auto-save/")
(def-path! test start "test/")
(def-path! libs core "libs/")
(def-path! packages layer "packages/")

(defconst user-home-directory
	(getenv "HOME"))
(defconst pcache-directory
	(concat aero-cache-directory "pcache/"))
(unless (file-exists-p aero-cache-directory)
	(make-directory aero-cache-directory))

(mapc 'add-to-load-path
			`(,aero-core-directory
				,aero-layer-directory
				,aero-libs-directory
				,aero-packages-directory))


;; Core functionality

(defun aero/bootstrap-package ()
	"Bootstrap `use-package' and set up for use"
	(setq package-enable-at-startup nil)
	(let ((default-directory "~/.emacs.d/elpa"))
		(unless (file-directory-p default-directory)
			(make-directory default-directory))
		(normal-top-level-add-subdirs-to-load-path))
	(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
													 ("melpa" . "https://melpa.org/packages/")
													 ("melpa-stable" . "https://stable.melpa.org/packages/")))
	(package-initialize)
	(unless (package-installed-p 'use-package)
		(package-refresh-contents)
		(package-install 'use-package))
	(eval-when-compile
		(require 'use-package))
	(use-package package
		;; TODO check signature
		:config (setq package-check-signature nil))
	(use-package quelpa :ensure t
		:config
		(quelpa
		 '(quelpa-use-package
			 :fetcher git
			 :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
		(require 'quelpa-use-package)))

(defun aero/update-packages ()
	(interactive)
	(package-refresh-contents)
	(quelpa-upgrade))

(defgroup aero nil
	"Aero customizations"
	:group 'starter-kit
	:prefix 'aero/)

(defun aero/startup-echo-message ()
	(message "Aero is ready"))


;; init functions

(defun aero/load-libs ()
	"Load Aero libraries and utilities, which will be required later"

	(require 'subr-x)
	(require 'aero-util)
	(require 'aero-lib))

(defun aero/init ()
	"Perform startup initialization, including all comilation and loading"
	(aero/load-libs)
	(aero/bootstrap-package)
	(require 'aero-layers)
	(aero/load-layers)

	(setq aero-initialized t))

(defun aero/startup-hook ()
	"Post-init processing"
	(aero/add-hook!
	 'emacs-startup-hook
	 (require 'aero-rc)
	 (setq aero-initialized t)
	 (setq gc-cons-threshold (car aero/gc-cons)
				 gc-cons-percentage (cadr aero/gc-cons))
	 (global-font-lock-mode)))

(provide 'aero-core)

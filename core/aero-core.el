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


;;; core functionality and utils

(defun aero/bootstrap ()
	"Bootstrap `use-package', `quelpa' and major fixes, and set up for use"

	;; set up package
	(setq package-enable-at-startup nil)
	(let ((default-directory "~/.emacs.d/elpa"))
		(unless (file-directory-p default-directory)
			(make-directory default-directory))
		(normal-top-level-add-subdirs-to-load-path))
	(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
													 ("melpa" . "https://melpa.org/packages/")
													 ("melpa-stable" . "https://stable.melpa.org/packages/")
													 ("org" . "https://orgmode.org/elpa/")))
	(package-initialize)

	;; use-package
	(unless (package-installed-p 'use-package)
		(package-refresh-contents)
		(package-install 'use-package))
	(eval-when-compile
		(require 'use-package)
		(setq use-package-expand-minimally byte-compile-current-file))
	(use-package package
		;; TODO check signature
		:config
		(setq package-check-signature nil))

  (use-package use-package-ensure-system-package
  :ensure t)

	(use-package quelpa :ensure t
    :init
    ;; update packages manually later
    (setq quelpa-update-melpa-p nil)
		:config
		(quelpa
		 '(quelpa-use-package
			 :fetcher git
			 :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
		(require 'quelpa-use-package))

  (use-package gnu-elpa-keyring-update :ensure t
    :pin gnu))

(defun aero/update-packages ()
	(interactive)
	(package-refresh-contents)
	(quelpa-upgrade))



(defgroup aero nil
	"Aero customizations"
	:group 'starter-kit
	:prefix 'aero/)


;;; init functions

(defun aero/load-libs ()
	"Load Aero libraries and utilities, which will be required later"

	(require 'subr-x)
	(require 'aero-util)
	(require 'aero-lib))

(defun aero/init ()
	"Perform startup initialization, including all comilation and loading"
	(aero/bootstrap)
	(aero/load-libs)
	(require 'aero-layers)
	(aero/load-layers)

  ;; baise cette merde
  (setq-default custom-file "/dev/null")

	(setq aero-initialized t))

(defun aero/startup-hook ()
	"Post-init processing"
	(aero/add-hook!
	 'emacs-startup-hook
	 (require 'aero-rc)
	 (setq aero-initialized t)
	 (setq gc-cons-threshold (car (cadr aero/gc-cons))
				 gc-cons-percentage (cadr (cadr aero/gc-cons)))
	 (global-font-lock-mode)))

(provide 'aero-core)

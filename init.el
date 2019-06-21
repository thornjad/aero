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


;;; optimizations and version check

;; vérifier les erreurs dans ce fichier
(setq debug-on-error t)

;; Avoid garbage collection during startup.
(setq gc-cons-threshold 268435456
			gc-cons-percentage 0.6)

;; Turn off mouse interface and other styles
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Always load newest byte code
(setq load-prefer-newer t)

;; explicitly set utf-8 to avoid prompt from emacs
(prefer-coding-system 'utf-8)
;; silence ad-handle-definition without advised functions being redefined
(setq ad-redefinition-action 'accept)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))


;;; initialization

(defvar aero-initialized nil
	"Whether Aero has finished initialization")

;; disable file-name-handlers for a speed boost during startup
(let ((file-name-handler-alist nil))

	;; burn baby burn
	(require 'aero-core)
	(aero/init)
	(aero/startup-hook)

	;; no more debug please
	(setq debug-on-error nil)
	(aero/log-info "Aero is ready"))

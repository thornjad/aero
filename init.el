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

(setq-default user-full-name "Jade Michael Thornton"
              user-mail-address "jade@jmthornton.net")

(defvar aero/gc-cons '((#x10000000 0.6)
                       (#x100000 0.1))
  "High and normal values for gc.

During init and while the minibuffer is in use, gc is set to the high
value (256 MB) to avoid collection, temporarily trading space for
cycles. During normal execution, the normal value (1 MB) is used, only
slightly above the default of 800 KiB, to reverse the trade so we use
more cycles but less space.")

;; Avoid garbage collection during startup by increasing thresholds.
(let ((gc-cons-threshold (car (car aero/gc-cons)))
      (gc-cons-percentage (cadr (car aero/gc-cons))))

;;; set up load paths

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
    (aero/log-info "Aero is ready")))

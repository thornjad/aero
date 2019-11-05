;;; init.el --- Aero Emacs -*- lexical-binding: t; coding: utf-8; mode: emacs-lisp; -*-
;;
;; Copyright (c) 2017-2019 Jade Michael Thornton
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;;; Code:


;;; optimizations and fixes

;; verifier les erreurs dans ce fichier
(setq debug-on-error t)
(defvar aero/gc-cons '((#x20000000 0.6)
                       (#x100000 0.1))
  "High and normal values for gc.

During init and while the minibuffer is in use, gc is set to the high
value (256 MB) to avoid collection, temporarily trading space for
cycles. During normal execution, the normal value (1 MB) is used, only
slightly above the default of 800 KiB, to reverse the trade so we use
more cycles but less space.")

;; Avoid garbage collection during startup by increasing thresholds.
;; Also disable some other crap which would just slow us down.
(let ((gc-cons-threshold (car (car aero/gc-cons)))
      (gc-cons-percentage (cadr (car aero/gc-cons)))
      (file-name-handler-alist nil))

  ;; grab optimizations
  (when (< emacs-major-version 27)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))

  ;; Always load newest byte code
  (setq load-prefer-newer t)

  ;; disable vc (which is very slow to load). We'll use magit anyway
  (setq vc-handled-backends nil)

  ;; explicitly set utf-8 to avoid prompt from emacs
  (prefer-coding-system 'utf-8)
  ;; silence ad-handle-definition without advised functions being redefined
  (setq ad-redefinition-action 'accept)

  ;; Patch security vulnerability in Emacs versions older than 25.3
  (when (version< emacs-version "25.3")
    (with-eval-after-load "enriched"
      (defun enriched-decode-display-prop (start end)
        (list start end))))

  (setq-default user-full-name "Jade Michael Thornton"
                user-mail-address "jade@jmthornton.net")

;;; set up load paths

  ;; load-path functions, byte compiled for fun and profit
  (defalias 'add-to-load-path #[(dir) "\301\302\"\207" [dir add-to-list load-path] 3])
  (defalias 'add-to-load-path-if-exists #[(dir) "\301!\205	 \302!\207" [dir file-exists-p add-to-load-path] 2])

  ;; Trade memory for less cycles when using the minibuffer
  (add-hook 'minibuffer-setup-hook
			(lambda () (setq gc-cons-threshold (car (car aero/gc-cons)))))
  (add-hook 'minibuffer-exit-hook
			(lambda () (setq gc-cons-threshold (car (cadr aero/gc-cons)))))

  (defmacro aero-path (parent dir)
    "Simply return a concatenated path"
    `(expand-file-name (format "%s/%s" ,parent ,dir)))

  (setq user-init-file
        (or load-file-name (buffer-file-name)))
  (setq user-emacs-directory
        (file-name-directory user-init-file))

  (defconst aero-start-dir
    user-emacs-directory)
  (defconst aero-core-dir
    (aero-path aero-start-dir "core/"))
  (defconst aero-lib-dir
    (aero-path aero-start-dir "lib/"))
  (defconst aero-packages-dir
    (aero-path aero-lib-dir "packages/"))
  (defconst aero-layers-dir
    (aero-path aero-lib-dir "layers/"))
  (defconst aero-private-dir
    (aero-path aero-lib-dir "private/"))
  (defconst aero-cache-dir
    (aero-path aero-start-dir ".cache/"))
  (defconst aero-autosave-dir
    (aero-path aero-cache-dir "auto-save/"))

  (defconst pcache-directory
    (concat aero-cache-dir "pcache/"))
  (unless (file-exists-p aero-cache-dir)
    (make-directory aero-cache-dir))

  (mapc 'add-to-load-path-if-exists
        `(,aero-core-dir
          ,aero-layers-dir
          ,aero-private-dir
          ,aero-lib-dir
          ,aero-packages-dir))

  ;; also add all packages to load path
  (let ((default-directory
          (format "%s/%s" user-emacs-directory "lib/packages/")))
    (normal-top-level-add-subdirs-to-load-path))


;;; initialization

  ;; burn baby burn
  (require 'aero-core)
  (declare-function aero/init "aero-core")
  (aero/init)

  ;; no more debug please
  (setq debug-on-error nil)
  (declare-function aero/log-info "aero-util")
  (aero/log-info "Aero has initialized"))

;;; init.el ends here

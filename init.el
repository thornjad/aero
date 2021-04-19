;;; init.el --- Aero Emacs -*- lexical-binding: t; coding: utf-8; mode: emacs-lisp; -*-
;;
;; Copyright (c) 2016-2021 Jade Michael Thornton
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

;; Catch-all version check
(when (or (member "--no-version-check" command-line-args)
          (version< emacs-version "28"))
  (error "Aero requires at least Emacs version 28. Please upgrade or use --no-version-check"))


;;; Core functionality

(defgroup aero nil
  "Aero customizations"
  :group 'starter-kit
  :prefix 'aero/)

(defvar aero-local-init-file-c
  (expand-file-name "init.local.elc" user-emacs-directory)
  "Local init file, loaded at the end of init.
Useful for adding or overriding settings and functions for the local environment
only. This file is not part of Aero proper, and is not shared.")
(defvar aero-local-init-file
  (expand-file-name "init.local.el" user-emacs-directory)
  "Non-compiled version of `aero-local-init-file-c'")

(defun aero/bootstrap ()
  "Bootstrap `straight', `use-package' and major components, and set up for use"

  (with-eval-after-load 'gnutls
    (eval-when-compile (require 'gnutls))

    ;; Do not allow insecure TLS connections.
    (setq gnutls-verify-error t)

    ;; Bump the required security level for TLS to an acceptably modern
    ;; value.
    (setq gnutls-min-prime-bits 3072))

  ;; Set up vars straight will use
  (eval-when-compile
    (defvar straight-repository-branch)
    (defvar straight-check-for-modifications))
  (setq straight-repository-branch "develop"
        straight-check-for-modifications nil)

  ;; Built-in since Emacs 26
  (with-eval-after-load 'straight
    (add-to-list 'straight-built-in-pseudo-packages 'let-alist))

  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (setf (point) (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (require 'straight)
  (declare-function straight-use-package "straight.el")
  (defvar straight-use-package-by-default)
  (straight-use-package 'use-package)
  (require 'use-package)
	(setq straight-use-package-by-default t)

  (eval-when-compile
    (defvar use-package-expand-minimally)
    (defvar use-package-verbose))
  (setq use-package-expand-minimally byte-compile-current-file
        use-package-verbose init-file-debug))

(defun aero/load-libs ()
  "Load Aero libraries and utilities, which will be required later"

  (require 'subr-x)
  (require 'aero-lib))

(defun aero/load-layers ()
  "Load all Aero layers"
  (require 'aero-prelude)

  (if (boundp 'aero-layers-dir)
      ;; Run through all existing layers and load them by name. NOTE: this
      ;; assumes that all layers provide a package identical to their file name.
      (dolist (layer (directory-files aero-layers-dir nil "\\.el$"))
        (require (intern (string-trim-right layer ".el"))))
    (error "Cannot load layers because `aero-layers-dir' is not bound! This should never happen, what have you done??")))

(defun aero/init ()
  "Perform startup initialization, including all comilation and loading"
  (aero/bootstrap)
  (aero/load-libs)
  (aero/load-layers)

  ;; baise cette merde
  (setq-default custom-file "/dev/null")

  ;; settings
  (require 'aero-rc)

  ;; Load local init
  (unless (load aero-local-init-file-c 'noerror 'nomessage)
    (load aero-local-init-file 'noerror 'nomessage))

  (global-font-lock-mode t)
  (eval-when-compile (defvar aero/gc-cons)) ; defined in init.el
  (setq gc-cons-threshold (car (cadr aero/gc-cons))
        gc-cons-percentage (cadr (cadr aero/gc-cons)))
  (setq read-process-output-max #x1000000))


;;; optimizations and fixes

;; verifier les erreurs dans ce fichier
(setq debug-on-error t)
(defvar aero/gc-cons '((#x20000000 0.6) (#x1000000 0.1))
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

  ;; Always load newest byte code
  (setq load-prefer-newer t)

  ;; disable vc (which is very slow to load). We'll use magit anyway
  (setq vc-handled-backends nil)

  ;; explicitly set utf-8 to avoid prompt from emacs
  (prefer-coding-system 'utf-8)
  ;; silence ad-handle-definition without advised functions being redefined
  (setq ad-redefinition-action 'accept)

  (setq-default user-full-name "Jade Michael Thornton"
                user-mail-address "jade@jmthornton.net")

  (defsubst add-to-load-path-if-exists (dir)
    (when (file-exists-p dir)
      (add-to-list 'load-path dir)))

  ;; Trade memory for less cycles when using the minibuffer
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq gc-cons-threshold (car (car aero/gc-cons)))))
  (add-hook 'minibuffer-exit-hook
            (lambda () (setq gc-cons-threshold (car (cadr aero/gc-cons)))))

  (setq user-init-file
        (or load-file-name (buffer-file-name)))
  (setq user-emacs-directory
        (file-name-directory user-init-file))

	(defconst aero-lib-dir (expand-file-name "lib/" user-emacs-directory))
	(defconst aero-core-dir (expand-file-name "core/" aero-lib-dir))
	(defconst aero-packages-dir (expand-file-name "packages/" aero-lib-dir))
	(defconst aero-layers-dir (expand-file-name "layers/" aero-lib-dir))
	(defconst aero-etc-dir (expand-file-name "etc/" user-emacs-directory))
	(defconst aero-cache-dir (expand-file-name "cache/" aero-etc-dir))
	(defconst aero-autosave-dir (expand-file-name "auto-save/" aero-cache-dir))

  (unless (file-exists-p aero-cache-dir)
    (make-directory aero-cache-dir))
  (defconst pcache-directory (expand-file-name "pcache/" aero-cache-dir))

  (mapc 'add-to-load-path-if-exists
        `(,aero-core-dir
          ,aero-layers-dir
          ,aero-lib-dir
          ,aero-packages-dir))

  ;; also add all packages to load path
  (let ((default-directory aero-packages-dir))
    (normal-top-level-add-subdirs-to-load-path))

  ;; burn baby burn
  (aero/init)

  ;; Log warnings but don't display them
  (setq warning-minimum-level :error)
  ;; no more debug please
  (setq debug-on-error nil)
  ;; after init, this will just cause unnecessary slowness
  (setq load-prefer-newer nil))

;;; init.el ends here

;;; init.el --- Aero Emacs -*- lexical-binding: t; coding: utf-8; mode: emacs-lisp; -*-
;;
;; Copyright (c) 2016-2022 Jade Michael Thornton
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
;; Aero primary init file, from which all other configuration is loaded.
;;
;;; Code:

;; Catch-all version check, should be updated when we use a new feature that's not
;; backward-compatible.
(when (or (member "--no-version-check" command-line-args)
         (version< emacs-version "28"))
 (error "Aero requires at least Emacs version 28. Please upgrade or use --no-version-check"))


;;; Core functionality

(defgroup aero nil
  "Aero base configuration group."
  :group 'starter-kit
  :prefix 'aero/)

(defun aero/bootstrap ()
  "Bootstrap `straight', `use-package' and major components, and set up for use"

  ;; Get our on hooks early
  (require 'on (expand-file-name "lib/core/on.el" user-emacs-directory))

  (with-eval-after-load 'gnutls
    (eval-when-compile (require 'gnutls))
    (setq gnutls-verify-error t) ; Do not allow insecure TLS connections.
    (setq gnutls-min-prime-bits 3072)) ; Make TLS use an acceptable modern value

  ;; Use the more-cutting-edge develop branch of straight, and don't allow it to check for
  ;; modifications in every repo on Emacs init, saving some startup time.
  (eval-when-compile
    (defvar straight-repository-branch)
    (defvar straight-check-for-modifications))
  (setq straight-repository-branch "develop"
        straight-check-for-modifications nil)

  ;; Tell straight that let-alist is a built-in package now, so it doesn't need to be checked if we
  ;; (or more likely any dependency) try to pull it in.
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

  ;; Bootstrap use-package
  (require 'straight)
  (declare-function straight-use-package "straight.el")
  (defvar straight-use-package-by-default)
  (straight-use-package 'use-package)
  (require 'use-package)
  (setq straight-use-package-by-default t)

  ;; Only expand minimally if we're byte-compiling, and only use verbose if we're in --debug-init.
  (eval-when-compile
    (defvar use-package-expand-minimally)
    (defvar use-package-verbose))
  (setq use-package-expand-minimally byte-compile-current-file
        use-package-verbose init-file-debug))

(defun aero/load-layers ()
  "Load all Aero layers.

A layer is a valid ELisp file which lives in `aero-layers-dir'. Provided package names MUST match their filename exactly."
  (require 'aero-prelude)
  (if (boundp 'aero-layers-dir)
      (dolist (layer (directory-files aero-layers-dir nil "^[^#]*\\.el$"))
        (require (intern (string-trim-right layer ".el"))))
    (error "Cannot load layers because `aero-layers-dir' is not bound! This should never happen, what have you done??")))

(defun aero/init ()
  "Perform startup initialization, including all comilation and loading"
  (aero/bootstrap)
  (require 'subr-x)
  (require 'aero-lib)
  (aero/load-layers)
  (setq-default custom-file "/dev/null") ; Don't use customization system
  (require 'aero-rc) ; Core settings

  ;; Load local init if it exists
  (load (expand-file-name "init.local" user-emacs-directory) t t)

  ;; Expand GC parameters so we use fewer, larger collections instead of more, smaller ones. On
  ;; modern systems with plenty of RAM, this should speed up Emacs slightly, at least in theory.
  ;; This is controversial, but I figure it makes enough sense to keep in here.
  (eval-when-compile (defvar aero/gc-cons))
  (setq gc-cons-threshold (car (cadr aero/gc-cons))
        gc-cons-percentage (cadr (cadr aero/gc-cons)))
  (setq read-process-output-max #x1000000))


;;; optimizations and fixes

(defvar aero/gc-cons '((#x50000000 1.0) (#x3000000 0.1))
  "High and normal values for gc.

During init and while the minibuffer is in use, gc is set to the high value to avoid collection,
temporarily trading space for cycles. During normal execution, the normal value (48 MiB) is used, a
bit above the default of 800 KiB, to reverse the trade so we use more cycles but less space, but not
too little space.")

;; Avoid garbage collection during startup by increasing thresholds.
;; Also disable some other crap which would just slow us down.
(let ((gc-cons-threshold (car (car aero/gc-cons)))
      (gc-cons-percentage (cadr (car aero/gc-cons)))
      (file-name-handler-alist nil))

  ;; BUT, trade memory for less cycles when using the minibuffer
  (add-hook 'minibuffer-setup-hook (lambda () (setq gc-cons-threshold (car (car aero/gc-cons)))))
  (add-hook 'minibuffer-exit-hook (lambda () (setq gc-cons-threshold (car (cadr aero/gc-cons)))))

  (setq debug-on-error t) ; verifier les erreurs dans ce fichier
  (setq load-prefer-newer t) ; Load newest code during init
  (prefer-coding-system 'utf-8) ; Just in case early-init missed this one, or old Emacs
  (setq ad-redefinition-action 'accept) ; Accept advice redefinition without complaining

  ;; Define load-file directories
  (setq user-init-file (or load-file-name (buffer-file-name)))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (defconst aero-lib-dir (expand-file-name "lib/" user-emacs-directory))
  (defconst aero-core-dir (expand-file-name "core/" aero-lib-dir))
  (defconst aero-packages-dir (expand-file-name "packages/" aero-lib-dir))
  (defconst aero-layers-dir (expand-file-name "layers/" aero-lib-dir))
  (defconst aero-etc-dir (expand-file-name "etc/" user-emacs-directory))
  (defconst aero-cache-dir (expand-file-name "cache/" aero-etc-dir))
  (defconst aero-autosave-dir (expand-file-name "auto-save/" aero-cache-dir))
  (defconst pcache-directory (expand-file-name "pcache/" aero-cache-dir))
  (unless (file-exists-p aero-cache-dir)
    (make-directory aero-cache-dir))

  ;; Actually add them all to the load-path.
  (defsubst add-to-load-path-if-exists (dir)
    (when (file-exists-p dir) (add-to-list 'load-path dir)))
  (mapc 'add-to-load-path-if-exists
        `(,aero-core-dir ,aero-layers-dir ,aero-lib-dir ,aero-packages-dir))

  ;; also add all packages to load path
  (let ((default-directory aero-packages-dir))
    (normal-top-level-add-subdirs-to-load-path))

  ;; burn baby burn
  (aero/init)

  (setq warning-minimum-level :error) ; Log warnings but don't let them pop up
  (setq debug-on-error nil) ; Disable debug for normal runtime
  (setq load-prefer-newer nil))

;;; init.el ends here

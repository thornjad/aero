;;; init.el --- Aero Emacs -*- lexical-binding: t; coding: utf-8; mode: emacs-lisp; -*-
;;
;; Copyright (c) 2016-2023 Jade Michael Thornton
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
  "Bootstrap major components and set up for use"

  ;; Get our on hooks early
  (require 'on (expand-file-name "lib/localpackages/on.el" user-emacs-directory))

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
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (setf (point) (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

	(require 'use-package)

  (and
	 (and (functionp 'module-load) (bound-and-true-p module-file-suffix))
   (require 'treesit nil t))

  ;; Only expand minimally if we're byte-compiling, and only use verbose if we're in --debug-init.
  (eval-when-compile
    (defvar use-package-expand-minimally)
    (defvar use-package-compute-statistics)
    (defvar use-package-minimum-reported-time)
    (defvar use-package-verbose))
  (setq use-package-expand-minimally byte-compile-current-file
        use-package-compute-statistics nil ; t then `use-package-report' to find packages not used
        use-package-minimum-reported-time 0.1
        use-package-verbose init-file-debug))

(defun aero/load-layers ()
  "Load all Aero layers.

A layer is a valid ELisp file which lives in `aero-layers-dir'. Provided package names MUST match their filename exactly."
  (require 'aero-prelude) ; Foundational packages
  (require 'aero-ui) ; Core theming, loaded now so we have less of a flash of basic Emacs

  ;; The rest of the layers need only exist in the `aero-layers-dir'. NOTE: layer must `provide' a
  ;; package matching its file name.
  (dolist (layer (directory-files aero-layers-dir nil "^[^#]*\\.el$"))
    (require (intern (string-trim-right layer ".el"))))

  ;; Core settings, loaded last to override layer settings
  (require 'aero-rc))

(defun aero/init ()
  "Perform startup initialization, including all comilation and loading"
  (aero/bootstrap)

  ;; Packages used by most stuff
  (require 'subr-x)
  (require 'aero-lib)

  (aero/load-layers)
  (setq-default custom-file "/dev/null") ; Don't use customization system

  ;; Load local init if it exists
  (load (expand-file-name "init.local" user-emacs-directory) t t)

  ;; Expand GC parameters so we use fewer, larger collections instead of more, smaller ones. On
  ;; modern systems with plenty of RAM, this should speed up Emacs slightly, at least in theory.
  ;; This is controversial, but I figure it makes enough sense to keep in here.
  (eval-when-compile (defvar aero/gc-cons))
  (setq gc-cons-threshold (car (cadr aero/gc-cons))
        gc-cons-percentage (cadr (cadr aero/gc-cons)))
  (setq read-process-output-max #x1000000))

;; Help debugging hidden errors
(defun aero/reraise-error (func &rest args)
  "Call function FUNC with ARGS and re-raise any error which occurs.
Useful for debugging post-command hooks and filter functions, which
normally have their errors suppressed."
  (condition-case err
      (apply func args)
    ((debug error) (signal (car err) (cdr err)))))

(defun aero/toggle-debug-on-hidden-errors (func)
  "Toggle hidden error debugging for function FUNC."
  (interactive "a")
  (cond
   ((advice-member-p #'aero/reraise-error func)
    (advice-remove func #'aero/reraise-error)
    (message "Debug on hidden errors disabled for %s" func))
   (t
    (advice-add func :around #'aero/reraise-error)
    (message "Debug on hidden errors enabled for %s" func))))


;;; optimizations and fixes

(defvar aero/gc-cons '((400000000 1.0) (100000000 0.1))
  "High and normal values for gc.

During init and while the minibuffer is in use, gc is set to the high value to avoid collection,
temporarily trading space for cycles, but not so high that we require OS paging. During normal
execution, the normal value (cadr) is used, a bit above the default of 800 KiB, to reverse the trade
so we use more cycles but less space, but not too little space.")

;; Avoid garbage collection during startup by increasing thresholds.
;; Also disable some other crap which would just slow us down.
(let ((gc-cons-threshold (car (car aero/gc-cons)))
      (gc-cons-percentage (cadr (car aero/gc-cons))))
	;; NOTE to future self, Doom has an optimization where `file-name-handler-alist' is set to nil
	;; during startup because many IO functions consult it needlessly. However, this stops Emacs from
	;; falling back to *.el.gz files if it can't find native- or byte-compiled versions of a package.
	;; This breaks often enough that it's not worth it to copy this behavior.

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
  (defconst aero-layers-dir (expand-file-name "layers/" aero-lib-dir))
  (defconst aero-etc-dir (expand-file-name "etc/" user-emacs-directory))
  (defconst aero-cache-dir (expand-file-name "cache/" aero-etc-dir))
  (defconst pcache-directory (expand-file-name "pcache/" aero-cache-dir))
  (unless (file-exists-p aero-cache-dir)
    (make-directory aero-cache-dir))

  ;; Actually add them all to the load-path.
  (defsubst add-to-load-path-if-exists (dir)
    (when (file-exists-p dir) (add-to-list 'load-path dir)))
  (mapc 'add-to-load-path-if-exists
        `(,aero-core-dir ,aero-layers-dir ,aero-lib-dir))

  ;; Do garbage collection when I'm not actively doing anything
  (run-with-idle-timer 7 t 'garbage-collect)

  ;; burn baby burn
  (aero/init)

  (setq warning-minimum-level :error) ; Log warnings but don't let them pop up
  (setq debug-on-error nil))

;;; init.el ends here

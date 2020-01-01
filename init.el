;;; init.el --- Aero Emacs -*- lexical-binding: t; coding: utf-8; mode: emacs-lisp; -*-
;;
;; Copyright (c) 2017-2020 Jade Michael Thornton
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
(when (version< emacs-version "28")
  (error "Aero requires at least Emacs version 28"))


;;; optimizations and fixes

;; verifier les erreurs dans ce fichier
(setq debug-on-error t)
(defvar aero/gc-cons '((#x20000000 0.6) (#x100000 0.1))
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

  (defalias 'add-to-load-path #[(dir) "\301\302\"\207" [dir add-to-list load-path] 3])
  (defalias 'add-to-load-path-if-exists #[(dir) "\301!\205   \302!\207" [dir file-exists-p add-to-load-path] 2])

  ;; Trade memory for less cycles when using the minibuffer
  (add-hook 'minibuffer-setup-hook
            (lambda () (setq gc-cons-threshold (car (car aero/gc-cons)))))
  (add-hook 'minibuffer-exit-hook
            (lambda () (setq gc-cons-threshold (car (cadr aero/gc-cons)))))

  (setq user-init-file
        (or load-file-name (buffer-file-name)))
  (setq user-emacs-directory
        (file-name-directory user-init-file))

  (defconst aero-core-dir
    (expand-file-name (format "%s/%s" user-emacs-directory "core/")))
  (defconst aero-lib-dir
    (expand-file-name (format "%s/%s" user-emacs-directory "lib/")))
  (defconst aero-packages-dir
    (expand-file-name (format "%s/%s" aero-lib-dir "packages/")))
  (defconst aero-layers-dir
    (expand-file-name (format "%s/%s" aero-lib-dir "layers/")))
  (defconst aero-private-dir
    (expand-file-name (format "%s/%s" aero-lib-dir "private/")))
  (defconst aero-cache-dir
    (expand-file-name (format "%s/%s" user-emacs-directory ".cache/")))
  (defconst aero-autosave-dir
    (expand-file-name (format "%s/%s" aero-cache-dir "auto-save/")))

  (unless (file-exists-p aero-cache-dir)
    (make-directory aero-cache-dir))
  (defconst pcache-directory
    (concat aero-cache-dir "pcache/"))

  (mapc 'add-to-load-path-if-exists
        `(,aero-core-dir
          ,aero-layers-dir
          ,aero-private-dir
          ,aero-lib-dir
          ,aero-packages-dir))

  (setq custom-theme-directory
        (expand-file-name
         (format "%s/%s" user-emacs-directory "etc/themes/")))

  ;; also add all packages to load path
  (let ((default-directory
          (format "%s/%s" user-emacs-directory "lib/packages/")))
    (normal-top-level-add-subdirs-to-load-path))

  ;; burn baby burn
  (require 'aero-core)
  (declare-function aero/init "aero-core")
  (aero/init)

  ;; no more debug please
  (setq debug-on-error nil)
  ;; after init, this will just cause unnecessary slowness
  (setq load-prefer-newer nil)
  (declare-function aero/log-info "aero-util")
  (aero/log-info "Aero has initialized"))

;;; init.el ends here

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
  "Bootstrap `use-package' and major components, and set up for use"

  (setq package-enable-at-startup nil)
  (let ((default-directory "~/.emacs.d/elpa"))
    (unless (file-directory-p default-directory)
      (make-directory default-directory))
    (normal-top-level-add-subdirs-to-load-path))

  ;; (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")))

  (when (version< emacs-version "26.3")
    ;; Emacs before 26.3 has a bug in its TLS implementation which breaks
    ;; synchonous TLS 1.3-only connections, such as GNU ELPA (as of July 2019,
    ;; at least). By setting this variable, we disable TLS 1.3 entirely. The
    ;; better option is to use Emacs 27+ and/or Remacs. See
    ;; https://debbugs.gnu.org/34341
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

  (package-initialize t)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-expand-minimally byte-compile-current-file
        use-package-verbose init-file-debug)

  (use-package use-package-ensure-system-package :ensure t)

  (use-package gnu-elpa-keyring-update :ensure t
    :pin gnu))



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
  (aero/load-private)

  ;; baise cette merde
  (setq-default custom-file "/dev/null")

  ;; settings
  (require 'aero-rc)

  (global-font-lock-mode)
  (setq gc-cons-threshold (car (cadr aero/gc-cons))
        gc-cons-percentage (cadr (cadr aero/gc-cons)))
  (setq aero-initialized t))

(provide 'aero-core)

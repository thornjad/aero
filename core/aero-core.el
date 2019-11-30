;; -*- lexical-binding: t -*-
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


;;; core functionality and utils

(defun aero/bootstrap ()
  "Bootstrap `straight', `use-package' and major components, and set up for use"

  (with-eval-after-load 'gnutls

    (when (or (version< emacs-version "26.3") (version< emacs-version "27.1"))
      ;; Emacs before 26.3 has a bug in its TLS implementation which breaks
      ;; synchonous TLS 1.3-only connections, such as GNU ELPA (as of July 2019,
      ;; at least). By setting this variable, we disable TLS 1.3 entirely. The
      ;; better option is to use Emacs 27+ and/or Remacs. See
      ;; https://debbugs.gnu.org/34341
      ;; Unfortunately, it appears that Remacs 27.0.50 also has this problem, so
      ;; we make a guess that 27.1 will have it fixed. It's more likely that
      ;; some patch version will contain this fix, but we'll burn that bridge
      ;; when get to it.
      (eval-when-compile (defvar gnutls-algorithm-priority))
      (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

    (eval-when-compile
      (require 'gnutls))

    ;; Do not allow insecure TLS connections.
    (setq gnutls-verify-error t)

    ;; Bump the required security level for TLS to an acceptably modern
    ;; value.
    (setq gnutls-min-prime-bits 3072))

  ;; if watchexec and Python are installed, use file watchers to detect package
  ;; modifications. This saves time at startup. Otherwise, use the ever-reliable
  ;; find(1).
  (eval-when-compile (defvar straight-check-for-modifications))
  (if (and (executable-find "watchexec")
         (executable-find "python3"))
      (setq straight-check-for-modifications '(watch-files find-when-checking))
    (setq straight-check-for-modifications
          '(find-at-startup find-when-checking)))

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
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (eval-when-compile (declare-function straight-use-package "straight.el"))
  (straight-use-package 'use-package)
  (require 'use-package)

  (setq use-package-expand-minimally byte-compile-current-file
        use-package-verbose init-file-debug)

  (use-package use-package-ensure-system-package :straight t)
  (use-package gnu-elpa-keyring-update :straight t))


;;; groups and vars

(defgroup aero nil
  "Aero customizations"
  :group 'starter-kit
  :prefix 'aero/)

(defvar aero-local-init-file
  (expand-file-name "init.local.el" user-emacs-directory)
  "Local init file, loaded at the end of init.
Useful for adding or overriding settings and functions for the local environment
only. This file is not part of Aero proper, and is not shared.")


;;; init functions

(defun aero/load-libs ()
  "Load Aero libraries and utilities, which will be required later"

  (require 'subr-x)
  (require 'aero-lib))

(defun aero/init ()
  "Perform startup initialization, including all comilation and loading"
  (aero/bootstrap)
  (aero/load-libs)
  (require 'aero-layers)
  (eval-when-compile (declare-function aero/load-layers "aero-layers"))
  (aero/load-layers)

  ;; baise cette merde
  (setq-default custom-file "/dev/null")

  ;; settings
  (require 'aero-rc)

  ;; Load local init
  (load aero-local-init-file 'noerror 'nomessage)

  (global-font-lock-mode)
  (eval-when-compile (defvar aero/gc-cons)) ; defined in init.el
  (setq gc-cons-threshold (car (cadr aero/gc-cons))
        gc-cons-percentage (cadr (cadr aero/gc-cons))))


;;; environment

(defvar aero--env-setup-p nil
  "Non-nil if `aero-env-setup' has completed at least once.")

(defun aero/env-setup (&optional again)
  "Load ~/.profile and set environment variables exported therein. Only do this
once, unless AGAIN is non-nil."
  (interactive (list 'again))
  ;; No need to worry about race conditions because Elisp isn't concurrent yet
  (unless (and aero--env-setup-p (not again))
    (let ((profile-file "~/.profile")
          (buf-name " *aero-env-output*"))
      (when (and profile-file
               (file-exists-p profile-file)
               (executable-find "python"))
        (ignore-errors (kill-buffer buf-name))
        (with-current-buffer (get-buffer-create buf-name)
          (let* ((python-script
                  (expand-file-name "scripts/print_env.py" user-emacs-directory))
                 (delimiter (md5
                             (format
                              "%s%s%s%s"
                              (system-name) (emacs-pid) (current-time) (random))))
                 (sh-script (format ". %s && %s %s"
                                    (shell-quote-argument
                                     (expand-file-name profile-file))
                                    (shell-quote-argument python-script)
                                    (shell-quote-argument delimiter)))
                 (return (call-process "sh" nil t nil "-c" sh-script))
                 (found-delimiter
                  (progn
                    (goto-char (point-min))
                    (search-forward delimiter nil 'noerror))))
            (if (and (= 0 return) found-delimiter)
                (let* ((results (split-string
                                 (buffer-string) (regexp-quote delimiter)))
                       (results (cl-subseq results 1 (1- (length results)))))
                  (if (cl-evenp (length results))
                      (progn
                        (cl-loop for (var value) on results by #'cddr do
                                 (setenv var value)
                                 (when (string= var "PATH")
                                   (setq exec-path (append
                                                    (parse-colon-path value)
                                                    (list exec-directory)))))
                        (setq aero--env-setup-p t))
                    (message
                     "Loading %s produced malformed result; see buffer %S"
                     profile-file
                     buf-name)))
              (message "Failed to load %s; see buffer %S"
                       profile-file
                       buf-name))))))))

(provide 'aero-core)

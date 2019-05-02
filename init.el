;;; init.el --- Aero Initialization File
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; NOTE: without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(defconst emacs-start-time (current-time))
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
(load (concat (file-name-directory load-file-name)
              "core/core-versions.el")
      nil (not init-file-debug))
(load (concat (file-name-directory load-file-name)
              "core/core-load-paths.el")
      nil (not init-file-debug))
(load (concat spacemacs-core-directory "core-dumper.el")
      nil (not init-file-debug))

(if (not (version<= aero-emacs-min-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "Aero requires Emacs version %s or above.")
           emacs-version aero-emacs-min-version)
  ;; Disable file-name-handlers for a speed boost during init
  (let ((file-name-handler-alist nil))
    (require 'core-spacemacs)
    (spacemacs|unless-dumping
      (when (boundp 'load-path-backup)
        (setq load-path load-path-backup)))
    (configuration-layer/load-lock-file)
    (spacemacs/init)
    (configuration-layer/stable-elpa-download-tarball)
    (configuration-layer/load)
    (aero-buffer/display-startup-note)
    (spacemacs/setup-startup-hook)
    (spacemacs|unless-dumping
      (global-font-lock-mode)
      (global-undo-tree-mode t)
      (winner-mode t))
    (when (and dotspacemacs-enable-server (not (spacemacs-is-dumping-p)))
      (require 'server)
      (when dotspacemacs-server-socket-dir
        (setq server-socket-dir dotspacemacs-server-socket-dir))
      (unless (server-running-p)
        (message "Starting a server...")
        (server-start)))
    (spacemacs|when-dumping-strict
      (setq load-path-backup load-path)
      ;; disable undo-tree to prevent from segfaulting when loading the dump
      (global-undo-tree-mode -1)
      (setq spacemacs-dump-mode 'dumped)
      (garbage-collect))))

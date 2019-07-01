;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package magit :ensure t
	:commands (magit-blame
             magit-commit
             magit-commit-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-fetch-popup
             magit-init
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-revert
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
  :init
	(general-define-key
	 :states '(normal emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"
	 "gs" 'magit-status
	 "gS" 'magit-stage-file
	 "gU" 'magit-unstage-file
	 "gb" 'magit-blame)

	:config
	(global-git-commit-mode t)
  (magit-auto-revert-mode nil)
  (setq magit-completing-read-function 'ivy-completing-read
        magit-buffer-name-format "%x%M%v: %t%x")
  (defadvice magit-diff (after switch-to-diff activate)
    (other-window 1))
	(use-package magit-todos :ensure t)
	(use-package evil-magit :ensure t))

(use-package ediff
  :commands (ediff ediff3)
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package gitlab-ci-mode :ensure t)


;;; project management

(use-package cruiser-find-file
  :disabled t
  :load-path aero-packages-directory
  :config
  (general-define-key
   :states '(normal motion emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "p" '(:ignore t :which-key "project")
   "pf" '(cruiser-find-file :which-key "find file")))

(provide 'aero-git)

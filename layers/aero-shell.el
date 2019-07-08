;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs


;;; eshell

(use-package eshell
  :commands eshell
  :config
  (setq-default eshell-aliases-file (concat user-emacs-directory "eshell-aliases")
                eshell-save-history-on-exit t
                eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  (setq-default
   eshell-prompt-function

   (lambda ()
     (concat
      "\n"
      (propertize " ┌─── " 'face '(:foreground "green"))
      (propertize (eshell/pwd) 'face '(:weight ultra-bold))
      "\n"
      (propertize " └─ λ " 'face '(:foreground "green"))))))

;;; shell scripting

(use-package sh-script :defer t
  :mode "\\.sh\\'\\.bash\\'\\.zsh\\'"
  :config
  (setq shell-file-name "/usr/local/bin/bash")

  (use-package company-shell
    :ensure t
    :config
    (add-hook
     'sh-mode-hook
     (lambda ()
       (set-local-company-backends! 'company-shell))))

  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end))))

  (defun sh-cleanup-line ()
    (interactive)
    (let* ((beg (line-beginning-position)))
      (save-excursion
        (end-of-line)
        (while (re-search-backward "--\\||\\|([><])\{1,2\}" beg t)
          (insert "\\")
          (newline-and-indent))
        (indent-paragraph)))))

(provide 'aero-shell)

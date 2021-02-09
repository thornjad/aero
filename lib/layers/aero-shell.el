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
  (require 'evil)
  (defvar evil-move-cursor-back)
  (defvar eshell-save-history-on-exit)
  (defvar eshell-history-size)
  (defvar eshell-ls-initial-args)
  (defvar eshell-cmpl-dir-ignore)
  (defvar eshell-visual-commands)
  (defvar eshell-visual-subcommands)

  (setq
   eshell-save-history-on-exit t
   eshell-buffer-maximum-lines 12000
   eshell-history-size 500
   eshell-ls-initial-args "-lah"
   eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
   eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                            "ncftp" "pine" "tin" "trn" "elm" "vim"
                            "nmtui" "alsamixer" "htop" "el" "elinks"
                            "ssh" "nethack" "dtop" "dstat")
   eshell-visual-subcommands '(("git" "log" "diff" "show"))
   pcomplete-ignore-case t)

  (let ((default-directory (expand-file-name "~")))
    (setq eshell-path-env (getenv "PATH")))

  ;; doesn't handle less too well
  (setenv "PAGER" "cat")
  (setenv "TERM" "xterm-256color")

  (add-hook
   'eshell-mode-hook
   (lambda ()
     (setq-local evil-move-cursor-back nil)
     (setq-local scroll-margin 0)))

  (defun eshell/cds ()
    "Change directory to git project root."
    (eshell/cd (locate-dominating-file default-directory ".git")))
  (defun eshell/clear ()
    (recenter 0))
  (defun eshell/magit ()
    "Open magit-status in the current directory."
    (interactive)
    (magit-status-setup-buffer default-directory))

  (dolist (x '(('e . (lambda (pattern)
                       (if (stringp pattern)
                           (find-file pattern)
                         (mapc #'find-file
                               (mapcar #'expand-file-name pattern)))))

               ('ee . #'find-file-other-window)
               ('eshell/la . #'eshell/ls)
               ('eshell/g . #'eshell/git)))
    (eval `(defalias ,(car x) ,(cdr x)))))


;;; term

(use-package term
  :commands (term
             term-bash)
  :init
  (add-hook
   'term-mode-hook
   (lambda ()
     (setq-local evil-move-cursor-back nil)
     (setq-local scroll-margin 0)))

  (general-def term-mode-map
    (kbd "M-h") 'windmove-left
    (kbd "M-l") 'windmove-right
    (kbd "M-p") 'term-previous-input
    (kbd "M-n") 'term-next-input)

  (dolist (x '("bash" "zsh" "ion" "cicada"))
    (eval
     `(defun ,(intern (concat "term-" x)) ()
        (interactive)
        (funcall-interactively #'term ,x))))

  (aero-leader-def
    "C-s" 'term-ion
    "Stt" 'term
    "Stb" 'term-bash
    "Stz" 'term-zsh
    "Stc" 'term-cicada))


;;; shell scripting

(use-package sh-script :defer t
  :mode ("\\.\\(sh\\|bash\\|zsh\\|zsh-theme\\)\\'" . sh-mode)
  :config
  (setq shell-file-name (cond
                         ((string= system-type "darwin") "/usr/local/bin/zsh")
                         (t "/usr/bin/zsh")))

  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end)))))

(use-package ion-mode
  :straight (ion-mode
             :host github :repo "iwahbe/ion-mode")
  :mode (("\\.ion\\'" . ion-mode)
         ("ion/initrc\\'" . ion-mode)))

(provide 'aero-shell)

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
      (propertize " └─ λ " 'face '(:foreground "green")))))

  ;; from aweshell by Andy Stewart
  (defun aero/validate-command ()
    "Validate command and colorize before send to eshell."
    (save-excursion
      (beginning-of-line)
      (re-search-forward (format "%s\\([^ \t\r\n\v\f]*\\)" eshell-prompt-regexp)
                         (line-end-position)
                         t)
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (command (match-string 1)))
        (when command
          (put-text-property
           beg end
           'face `(:foreground
                   ,(if
                        (or
                         ;; Command exists?
                         (executable-find command)
                         ;; Or command is an alias?
                         (seq-contains (eshell-alias-completions "") command)
                         ;; Or it is ../. ?
                         (or (equal command "..")
                             (equal command ".")
                             (equal command "exit"))
                         ;; Or it is a file in current dir?
                         (member (file-name-base command) (directory-files default-directory))
                         ;; Or it is a elisp function
                         (functionp (intern command)))
                        "#98C379"
                      "#FF0000")))
          (put-text-property beg end 'rear-nonsticky t)))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-hook 'post-command-hook #'aero/validate-command t t)))

  (defun aero/cat-with-syntax-highlight (filename)
    "Like cat(1) but with syntax highlighting."
    (let ((existing-buffer (get-file-buffer filename))
          (buffer (find-file-noselect filename)))
      (eshell-print
       (with-current-buffer buffer
         (if (fboundp 'font-lock-ensure)
             (font-lock-ensure)
           (with-no-warnings
             (font-lock-fontify-buffer)))
         (buffer-string)))
      (unless existing-buffer
        (kill-buffer buffer))
      nil))

  (advice-add 'eshell/cat :override #'aero/cat-with-syntax-highlight)

  (use-package eshell-did-you-mean
    :load-path aero-packages-directory
    :config
    (add-hook 'eshell-mode-hook
              (lambda ()
                (run-with-idle-timer
                 1 nil
                 #'(lambda ()
                     (eshell-did-you-mean-setup)
                     ))))))

;;; shell scripting

(use-package sh-script :defer t
  :mode
  (("\\.sh\\'" . shell-script-mode)
   ("\\.bash\\'" . shell-script-mode)
   ("\\.zsh\\'" . shell-script-mode)
   ("zlogin\\'" . shell-script-mode)
   ("zlogout\\'" . shell-script-mode)
   ("zpreztorc\\'" . shell-script-mode)
   ("zprofile\\'" . shell-script-mode)
   ("zshenv\\'" . shell-script-mode)
   ("zshrc\\'" . shell-script-mode))
  :config
  (setq shell-file-name "/usr/local/bin/bash")

  (use-package company-shell
    :ensure t
    :config
    (add-hook!
     'sh-mode-hook
     (set-local-company-backends! 'company-shell)))

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

(use-package xterm-color :ensure t)

(provide 'aero-shell)

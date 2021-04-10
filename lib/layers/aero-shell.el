;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019, 2021 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)


;;; eshell

(use-package eshell
  :after (general)
  :commands eshell
  :init
  (require 'em-smart)
  :config
  (require 'evil)
  (defvar evil-move-cursor-back)
  (defvar eshell-save-history-on-exit)
  (defvar eshell-history-size)
  (defvar eshell-ls-initial-args)
  (defvar eshell-cmpl-dir-ignore)
  (defvar eshell-visual-commands)
  (defvar eshell-visual-subcommands)

  ;; Ensure eshell doesn't override these
  (general-def term-mode-map
    (kbd "M-h") 'windmove-left
    (kbd "M-l") 'windmove-right
    (kbd "M-p") 'eshell-previous-input
    (kbd "M-n") 'eshell-next-input)

  (setq
   eshell-save-history-on-exit t
   eshell-buffer-maximum-lines 12000
   eshell-aliases-file (expand-file-name "eshell-alias" aero-etc-dir)
   eshell-history-size 500
   eshell-ls-initial-args "-lah"
   eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
   eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                            "ncftp" "pine" "tin" "trn" "elm" "vim"
                            "nmtui" "alsamixer" "htop" "el" "elinks"
                            "ssh" "nethack" "dtop" "dstat")
   eshell-visual-subcommands '(("git" "log" "diff" "show"))
   completion-ignore-case t)
  (setenv "PAGER" "bat")

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

  (defalias 'eshell/emacs 'find-file)
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
    (eval `(defalias ,(car x) ,(cdr x))))

  (defun eshell/ec (pattern)
    (if (stringp pattern)
        (find-file pattern)
      (mapc #'find-file (mapcar #'expand-file-name pattern))))
  (defalias 'e 'eshell/ec)
  (defalias 'ee 'find-file-other-window)

  (defun eshell/sudoec (file)
    (interactive)
    (if (file-remote-p file)
        ;; TODO
        (message "can't sudoec a remote file yet")
      (find-file (concat "/sudo::" (expand-file-name file)))))

  (defun eshell/icat (&rest args)
    "Display image(s)."
    (let ((elems (flatten-list args)))
      (while elems
        (eshell-printn
         (propertize " "
                     'display (create-image (expand-file-name (car elems)))))
        (setq elems (cdr elems))))
    nil)

  ;; So the history vars are defined
  (require 'em-hist)
  (if (boundp 'eshell-save-history-on-exit)
      ;; Don't ask, just save
      (setq eshell-save-history-on-exit t))

  (use-package esh-opt :straight nil
    :config
    (use-package em-cmpl :straight nil)
    (use-package em-prompt :straight nil)
    (use-package em-term :straight nil)

    (setq eshell-cmpl-cycle-completions nil
          ;; auto truncate after 12k lines
          eshell-buffer-maximum-lines 12000
          ;; history size
          eshell-history-size 500
          ;; buffer shorthand -> echo foo > #'buffer
          eshell-buffer-shorthand t
          ;; treat 'echo' like shell echo
          eshell-plain-echo-behavior t
          eshell-ls-initial-args "-lah")

    ;; Visual commands
    (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                                   "ncftp" "pine" "tin" "trn" "elm" "vim"
                                   "nmtui" "alsamixer" "htop" "el" "elinks"
                                   "ssh" "nethack" "dtop" "dstat"))
    (setq eshell-visual-subcommands '(("git" "log" "diff" "show")
                                      ("vagrant" "ssh")))))

(use-package eshell-prompt-extras
  :after (eshell esh-opt)
  :init
  (setq eshell-highlight-prompt t
        epe-git-dirty-char " *"
        eshell-prompt-function 'epe-theme-lambda)
  (autoload 'epe-theme-lambda "eshell-prompt-extras"))

(use-package eshell-did-you-mean
  :after (eshell)
  :config (eshell-did-you-mean-setup))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :functions (esh-autosuggest-mode))

;; Ensures editor commands open in the current Emacs session
(use-package with-editor
  :defines (with-editor-export-editor)
  :init
  (add-hook 'shell-mode-hook  'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))


;;; term

(use-package term :straight nil :after (general)
  :commands (term term-bash)
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

  (dolist (x '("bash" "zsh" "cicada"))
    (eval
    `(defun ,(intern (concat "term-" x)) ()
        (interactive)
        (funcall-interactively #'term ,x))))

  (aero-leader-def
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

(provide 'aero-shell)

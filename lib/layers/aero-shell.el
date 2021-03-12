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

  (setq aero/eshell-time-before-alert 5.0)

  (defun aero/eshell-precommand ()
    (interactive)
    (setq-local aero/eshell-command-start-time (current-time)))

  (defun aero/eshell-command-finished ()
    (interactive)
    (when (and (boundp 'aero/eshell-command-start-time)
               (> (float-time (time-subtract (current-time)
                                             aero/eshell-command-start-time))
                  aero/eshell-time-before-alert))
      (sauron-add-event major-mode
                        (if (zerop eshell-last-command-status)
                            3
                          4)
                        (format "EShell: command [%s] finished, status: %s"
                                eshell-last-command-name
                                eshell-last-command-status)
                        (lambda () (switch-to-buffer-other-window (buffer-name)))
                        nil)))
  (add-hook 'eshell-pre-command-hook #'aero/eshell-precommand)
  (add-hook 'eshell-post-command-hook #'aero/eshell-command-finished)

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
    (let ((elems (eshell-flatten-list args)))
      (while elems
        (eshell-printn
         (propertize " "
                     'display (create-image (expand-file-name (car elems)))))
        (setq elems (cdr elems))))
    nil)

  (use-package eshell-prompt-extras
    :init
    (progn
      (setq eshell-highlight-prompt nil
            epe-git-dirty-char " *"
            eshell-prompt-function 'epe-theme-multiline-with-status)))

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
  :straight (ion-mode :host github :repo "iwahbe/ion-mode")
  :mode (("\\.ion\\'" . ion-mode)
         ("ion/initrc\\'" . ion-mode)))

(provide 'aero-shell)

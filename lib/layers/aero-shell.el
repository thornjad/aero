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
   eshell-visual-subcommands '(("git" "log" "diff" "show")))

  ;; doesn't handle less too well
  (setenv "PAGER" "cat")
  (setenv "TERM" "xterm-256color")

  (add-hook
   'eshell-mode-hook
   (lambda () (setq-local evil-move-cursor-back nil)))

  (defun eshell/cds ()
    "Change directory to git project root."
    (eshell/cd (locate-dominating-file default-directory ".git")))
  (defalias 'eshell/la 'eshell/ls)
  (defun eshell/ec (pattern)
    (if (stringp pattern)
        (find-file pattern)
      (mapc #'find-file (mapcar #'expand-file-name pattern))))
  (defalias 'e 'eshell/ec)
  (defalias 'ee 'find-file-other-window)
  (defun eshell/clear ()
    (recenter 0))
  (defun eshell/magit ()
    "Open magit-status in the current directory."
    (interactive)
    (magit-status default-directory)))

(use-package xterm-color
  :disabled t
  :load-path "lib/packages/xterm-color/"
  :after eshell
  :commands (xterm-color-filter)
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook
   'shell-mode-hook
   (lambda ()
     ;; Disable font-locking in this buffer to improve performance
     (font-lock-mode -1)
     ;; Prevent font-locking from being re-enabled in this buffer
     (make-local-variable 'font-lock-function)
     (setq font-lock-function (lambda (_) nil))
     (add-hook
      'comint-preoutput-filter-functions
      'xterm-color-filter nil t)))

  (add-hook
   'eshell-mode-hook
   (lambda ()
     (setq xterm-color-preserve-properties t)
     (setenv "TERM" "xterm-256color")
     (add-to-list
      'eshell-preoutput-filter-functions
      'xterm-color-filter)
     (setq eshell-output-filter-functions
           (remove
            'eshell-handle-ansi-color
            eshell-output-filter-functions))))

  (setq-default compilation-environment '("TERM=xterm-256color"))

  (add-hook 'compilation-start-hook
            (lambda (proc)
              ;; We need to differentiate between compilation-mode buffers
              ;; and running as part of comint (which at this point we assume
              ;; has been configured separately for xterm-color)
              (when (eq (process-filter proc) 'compilation-filter)
                ;; This is a process associated with a compilation-mode buffer.
                ;; We may call `xterm-color-filter' before its own filter
                ;; function.
                (set-process-filter
                 proc
                 (lambda (proc string)
                   (funcall 'compilation-filter proc
                            (xterm-color-filter string))))))))


;;; term

(use-package term
  :commands (term)
  :functions (term-char-mode)
  :config
  (require 'evil)
  (defvar evil-move-cursor-back)

  ;; Much of the following syncing is adapted from evil-collection-term.el.

  (defun aero/term-switch-to-char-mode-on-insert ()
    "Switch to `term-char-mode' on insert state."
    (when (get-buffer-process (current-buffer))
      (term-char-mode)))

  (defun aero/term-sync-state ()
    "Sync `term-char-mode' and `term-line-mode' with evil's insert and normal
    states."
    (add-hook 'evil-insert-state-entry-hook
              #'aero/term-switch-to-char-mode-on-insert nil t)
    (add-hook 'evil-insert-state-exit-hook 'term-line-mode nil t))

  (defun aero/term-char-mode-insert ()
    "Switch to `term-char-mode' and insert state"
    (interactive)
    (term-char-mode)
    (evil-insert-state))

  (evil-set-initial-state 'term-mode 'insert)
  (add-hook 'term-mode-hook #'aero/term-sync-state)

  ;; keep evil from moving the cursor back, which is the default but not
  ;; appropriate in terminals
  (add-hook
   'term-mode-hook
   (lambda () (setq-local evil-move-cursor-back nil)))
  (add-hook
   'term-mode-hook
   (lambda ()
     (when (get-buffer-process (current-buffer))
       (term-char-mode))))

  ;; Evil has "C-" bindings that shadow regular terminal bindings, so lets
  ;; kill those. We keep "C-c" and "C-h".
  (general-def 'insert term-raw-map
    (kbd "C-a") 'term-send-raw
    (kbd "C-b") 'term-send-raw
    (kbd "C-d") 'term-send-raw
    (kbd "C-e") 'term-send-raw
    (kbd "C-f") 'term-send-raw
    (kbd "C-k") 'term-send-raw
    (kbd "C-l") 'term-send-raw
    (kbd "C-n") 'term-send-raw
    (kbd "C-o") 'term-send-raw
    (kbd "C-p") 'term-send-raw
    (kbd "C-q") 'term-send-raw
    (kbd "C-r") 'term-send-raw
    (kbd "C-s") 'term-send-raw
    (kbd "C-t") 'term-send-raw
    (kbd "C-u") 'term-send-raw
    (kbd "C-v") 'term-send-raw
    (kbd "C-w") 'term-send-raw
    (kbd "C-y") 'term-send-raw
    (kbd "C-z") 'term-send-raw
    (kbd "C-c C-d") 'term-send-eof
    (kbd "C-c C-z") 'term-stop-subjob)

  (general-def 'normal term-mode-map
    (kbd "C-c C-k") 'aero/term-char-mode-insert
    (kbd "RET") 'term-send-input
    "p" 'term-paste
    "[[" 'term-previous-prompt
    "]]" 'term-next-prompt
    (kbd "C-k") 'term-previous-prompt
    (kbd "C-j") 'term-next-prompt
    (kbd "M-p") 'term-previous-input
    (kbd "M-n") 'term-next-input))

;;; shell scripting

(use-package sh-script :defer t
  :mode "\\.sh\\'\\.bash\\'\\.zsh\\'"
  :config
  (setq shell-file-name "/usr/local/bin/bash")

  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end)))))

(provide 'aero-shell)

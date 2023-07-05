;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2023 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
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

(require 'aero-prelude)

;; Make files executable if the first file has a shebang
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(package! xterm-color :auto
  :commands (xterm-color-filter)
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun aero/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'aero/advice-compilation-filter))


;;; eshell

(package! eshell :builtin
  :after (general evil)
  :commands eshell
  :defines (evil-move-cursor-back
            eshell-save-history-on-exit
            eshell-history-size
            eshell-glob-case-insensitive
            eshell-ls-initial-args
            eshell-cmpl-dir-ignore
            eshell-visual-commands
            eshell-visual-subcommands)
  :functions (eshell-previous-input
              eshell-next-input)
  :config
  ;; Ensure eshell doesn't override these
  (define-key eshell-mode-map (kbd "M-h") 'windmove-left)
  (define-key eshell-mode-map (kbd "M-l") 'windmove-right)
  (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
  (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)

  (setq
   eshell-save-history-on-exit t
   eshell-buffer-maximum-lines 12000
   eshell-glob-case-insensitive t
   eshell-aliases-file (expand-file-name "eshell-alias" aero-etc-dir)
   eshell-history-size 350
   eshell-ls-initial-args "-lah"
   eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'"
   eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
                            "ncftp" "pine" "tin" "trn" "elm" "vim"
                            "nmtui" "alsamixer" "htop" "el" "elinks"
                            "ssh" "nethack" "dtop" "dstat" "docker-compose")
   eshell-visual-subcommands '(("git" "log" "diff" "show"))
   eshell-cmpl-cycle-completions nil ; tab cycles the completion list
   eshell-buffer-maximum-lines 12000 ; auto truncate after 12k lines
   eshell-history-size 500 ; history size
   eshell-buffer-shorthand t ; buffer shorthand -> echo foo > #'buffer
   eshell-plain-echo-behavior t ; treat 'echo' like shell echo
   eshell-banner-message '(format "%s %s\n"
                                  (propertize (format " %s " (string-trim (buffer-name)))
                                              'face 'mode-line-highlight)
                                  (propertize (current-time-string)
                                              'face 'font-lock-keyword-face))
   eshell-scroll-to-bottom-on-input 'all
   eshell-kill-processes-on-exit t
   eshell-hist-ignoredups t
   eshell-error-if-no-glob t  ; mimics zsh behavior
   completion-ignore-case t)

  ;; Enable autopairing in eshell
  (add-hook 'eshell-mode-hook #'smartparens-mode)

  ;; Try to load in PATH
  (let ((default-directory (expand-file-name "~")))
    (setq eshell-path-env (getenv "PATH")))

  ;; doesn't handle less too well
  ;; (setenv "PAGER" "cat")
  (setenv "PAGER" "bat")
  (setenv "TERM" "xterm-256color")

  ;; Remove hscroll-margin in shells, otherwise you get jumpiness when the
  ;; cursor comes close to the left/right edges of the window.
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local hscroll-margin 0
                                   evil-move-cursor-back nil)))

  ;; Use tab to cycle completions
  (add-hook 'eshell-mode-hook (lambda () (setq-local pcomplete-cycle-completions nil)))

  (defalias 'eshell/emacs 'find-file)

  (defun eshell/e (pattern)
    (if (stringp pattern)
        (find-file pattern)
      (mapc #'find-file (mapcar #'expand-file-name pattern))))

  ;; So the history vars are defined
  (require 'em-hist)
  (when (boundp 'eshell-save-history-on-exit)
    ;; Don't ask, just save
    (setq eshell-save-history-on-exit t))

  (eval-after-load 'esh-opt
		'(progn
       (require 'em-cmpl)
       (require 'em-prompt)
       (require 'em-term))))

(package! eshell-prompt-extras :auto
  :after (eshell)
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-multiline-with-status "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-multiline-with-status)))

(package! eshell-syntax-highlighting :auto
  :after eshell-mode
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(package! esh-help :auto
  :after eshell
  :config (setup-esh-help-eldoc))


;;; shell scripting

(package! sh-script :builtin :defer t
  :mode ("\\.\\(sh\\|bash\\|zsh\\|zsh-theme\\)\\'" . sh-mode)
  :config
  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end)))))

(provide 'aero-shell)

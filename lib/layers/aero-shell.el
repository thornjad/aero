;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2022 Jade Michael Thornton
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

;; Allow ANSI color escapes in compilation mode
;; (ignore-errors
;;   (require 'ansi-color)
;;   (defun colorize-compilation-buffer ()
;;     (when (eq major-mode 'compilation-mode)
;;       (let ((inhibit-read-only t))
;;         (ansi-color-apply-on-region compilation-filter-start (point-max)))))
;;   (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package xterm-color :straight t
  :commands (xterm-color-filter)
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun aero/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'aero/advice-compilation-filter))


;;; eshell

(use-package eshell
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
  (require 'em-smart)

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
     (setq-local evil-move-cursor-back nil
                 scroll-margin 0)))

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
  (when (boundp 'eshell-save-history-on-exit)
    ;; Don't ask, just save
    (setq eshell-save-history-on-exit t))

  (eval-after-load 'esh-opt
		'(progn
       (require 'em-cmpl)
       (require 'em-prompt)
       (require 'em-term)

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
                                         ("vagrant" "ssh"))))))


;;; vterm

;; NOTE: vterm requires libvterm-dev, which may not be installed. See
;; https://github.com/akermu/emacs-libvterm for full install instructions. Also requires shell-side
;; configuration.
(when (bound-and-true-p module-file-suffix)  ; Requires Emacs modules
  (use-package vterm :straight t
    :commands (vterm)
    :after (general)
    :custom
    (vterm-max-scrollback 5000)
    (vterm-kill-buffer-on-exit t)

    :init
    ;; HACK vterm clumsily forces vterm-module.so to compile when the package is loaded. This is
    ;; necessary to prevent compilation when use-package is evaluated during byte- or
    ;; native-compilation of _this_ file.
    (when noninteractive
      (advice-add #'vterm-module-compile :override #'ignore)
      (provide 'vterm-module))
    (aero-leader-def
      "Stv" 'vterm
      "S'" 'vterm)))

(use-package multi-vterm :straight t :defer t
  :after (vterm general)
  :config
	(add-hook 'vterm-mode-hook
			      (lambda ()
			        (setq-local evil-insert-state-cursor 'bar)
			        (evil-insert-state)))

  (aero-leader-def
    "`" 'multi-vterm-dedicated-toggle
    "p`" 'multi-vterm-project)

  (aero-mode-leader-def 'vterm-mode-map
    "c" 'multi-vterm
    "n" 'mutli-vterm-next
    "p" 'multi-vterm-prev))


;;; shell scripting

(use-package sh-script :defer t
  :mode ("\\.\\(sh\\|bash\\|zsh\\|zsh-theme\\)\\'" . sh-mode)
  :config
  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end)))))

(provide 'aero-shell)

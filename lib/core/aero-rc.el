;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2021 Jade Michael Thornton
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
;;
;;; Code:


(setq-default
 ;; general
 ring-bell-function 'ignore ; supprime cette putain de cloche.
 sentence-end-double-space nil ; the world will not go to shit today
 default-fill-column 100 ; i am mortal, not arthur whitney
 fill-column 100
 help-window-select t ; focus help window when opened
 kill-ring-max 5000 ; truncate kill ring after 5000 entries
 mark-ring-max 5000 ; truncate mark ring after 5000 entries
 kill-do-not-save-duplicates t
 apropos-do-all t ; apropos is apropos
 global-display-line-numbers-mode nil ; fuck line numbers
 gnutls-min-prime-bits 4096 ; 256 est absurde
 confirm-kill-emacs 'yes-or-no-p ; too easy to kill when looking for alt file
 switch-to-buffer-preserve-window-point t
 line-move-visual t ; move lines by display, not reality
 make-pointer-invisible t ; le curseur est une chienne
 auto-revert-interval 10 ; wait just a little longer (default is 5)
 pop-up-windows nil ; make new window for pop-ups
 shared-game-score-directory (expand-file-name "game-scores/" aero-etc-dir)
 idle-update-delay 2 ; default is 0.5
 bidi-display-reordering nil ; no need for bidirectional display

 ;; Defaults:
 ;; '("gnutls-cli --insecure -p %p %h"
 ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
 ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
 tls-program '("gnutls-cli -p %p %h"
               "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")

 ;; Scrolling
 ;; Emacs spends too much effort recentering the screen if you scroll the
 ;; cursor more than N lines past window edges (where N is the settings of
 ;; `scroll-conservatively'). This is especially slow in larger files
 ;; during large-scale scrolling commands. If kept over 100, the window is
 ;; never automatically recentered.
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t
 mouse-wheel-scroll-amount '(3 ((shift) . 1))
 pixel-resolution-fine-flag 1
 hscroll-margin 5
 hscroll-step 1
 ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
 ;; for tall lines.
 auto-window-vscroll nil
 mouse-wheel-progressive-speed nil ; don't accelerate TODO may not want this?
 comint-scroll-to-bottom-on-input t ; insert at bottom
 comint-scroll-to-bottom-on-output nil ; don't scroll on output
 comint-input-ignoredups t
 comint-prompt-read-only nil ; breaks shell-command sometimes
 eww-search-prefix "https://lite.duckduckgo.com/lite?q="

 ;; The newline pushes everything else to a non-rendered second line
 frame-title-format "Emacs"
 ns-use-proxy-icon nil ; remove icon from frame title in ns

 ;; startup with scratch
 inhibit-startup-screen t
 inhibit-splash-screen t
 inhibit-startup-echo-area-message t
 initial-buffer-choice (lambda ()
                         (unless (get-buffer "*dashboard*")
                           (get-buffer "*scratch*")))
 initial-major-mode 'text-mode
 initial-scratch-message (concat ";; Aero Emacs v" emacs-version
																 "." (number-to-string emacs-build-number)
                                 "\n"
                                 ";; Enfin, un putain de bon éditeur"
                                 "\n;;\n"
                                 ";; Booted in " (emacs-init-time "%.3f seconds")
                                 (format " with %d garbage collections" gcs-done)
                                 (when (boundp 'straight--profile-cache)
                                   (format "\n;; Initialized %d packages"
                                           (+ (hash-table-size straight--profile-cache)
                                              (if (bound-and-true-p package-alist)
                                                  (length package-activated-list)
                                                0))))
                                 "\n\n")

 ;; version control and saving
 use-package-verbose nil
 delete-old-versions -1 ; supprime les vieilles versions des fichiers
                                        ; sauvegardés
 backup-directory-alist `(("." . "~/.config/emacs/backups"))
 version-control t
 vc-follow-symlinks t
 git-commit-fill-column 72
 auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save-list/" t))
 save-interprogram-paste-before-kill t
 diff-switches "-u" ; unified diff by default

 ;; files
 confirm-nonexistent-file-or-buffer nil ; don't ask to create a buffer
 require-final-newline t
 load-prefer-newer t
 completion-ignore-case t ; ignorer la capitalisation
 read-file-name-completion-ignore-case t ; ignorer la capitalisation des fichiers
 delete-auto-save-files t ; auto-delete auto-save auto-files automatically

 ;; indentation
 indent-tabs-mode nil
 tab-width 2
 c-basic-offset 2
 cperl-indent-level 2
 css-indent-offset 2
 evil-shift-width 2
 js-basic-offset 2
 js-indent-level 2
 js-switch-indent-offset 2
 js-syntactic-mode-name nil ; just use normal mode name
 js2-basic-offset 2
 python-indent-offset 4
 rust-indent-offset 4
 sgml-basic-offset 2
 sh-basic-offset 2
 tcl-indent-level 2
 tcl-tab-always-indent t
 lua-indent-level 2
 web-mode-attr-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-markup-indent-offset 2
 tab-stop-list (number-sequence 2 200 2))

;; ensure lang is set properly
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; remplace yes no par y n
(defalias 'yes-or-no-p 'y-or-n-p)

;; make case statements indent properly
(with-eval-after-load 'prog-mode
  (c-set-offset 'case-label '++))

;; prevent savehist cpu hogging
(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Don't kill my scratch!"
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; Enable mouse in TTY
(unless (display-graphic-p)
  ;; activate mouse-based scrolling
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Trigger auto-fill after punctutation characters, not just whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; type to get rid of active selection
(delete-selection-mode t)

(save-place-mode 1)

(when (string= system-type "darwin")
	(setq-default dired-use-ls-dired nil))

;; ensure buffer names are unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defun display-startup-echo-area-message ()
  "Override ridiculous built-in crap."
  (message "Aero est prêt"))

(defun aero/compilation-finish (buf status)
  "Notify with status, then delete window and bury buffer if successful after 2 secs."
  (call-process "notify-send" nil nil nil
                "-t" "0" "-i" "emacs"
                "Compilation finished in Emacs"
                status)
  (when (string= (string-trim status) "finished")
    (bury-buffer buf)
    (run-at-time "2 sec" nil #'aero/delete-windows-on-if-exist buf)))
(setq compilation-finish-functions
      (append compilation-finish-functions
              '(aero/compilation-finish)))

;; Small compilation window
(defun aero/compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      ;; select bottom window
      (let ((bottom-window (selected-window))
            window-below)
        (while (setq window-below (window-in-direction 'below bottom-window))
          (setq bottom-window window-below))
        (select-window bottom-window))
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h (or compilation-window-height 12))))))))
(add-hook 'compilation-mode-hook 'aero/compilation-hook)

;; open some buffers in the same window
(add-to-list 'display-buffer-alist
             '("*Help*" display-buffer-same-window)
             '("*helpful*" display-buffer-same-window))

;; If we leave a buffer, set its mark as inactive
(transient-mark-mode 1)

(provide 'aero-rc)

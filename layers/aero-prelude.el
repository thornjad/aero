;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
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

(eval-when-compile
  (require 'cl-lib))


;; garder ma merde à jour

(use-package auto-package-update :ensure t
	:defines auto-package-update-maybe
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))


;; the general is here

;; TODO use :general keyword with use-package
(use-package which-key :ensure t
	:defines which-key-mode
	:config
	(which-key-mode))
(use-package general :ensure t
	:defines (general-define-key)
  :functions (general-imap
							general-emap
							general-nmap
							general-vmap
							general-mmap
							general-omap
							general-rmap
							general-iemap
							general-nvmap
							general-itomap
							general-otomap
							general-tomap
							general--sanitize-arglist
							general-normalize-hook-arglist
							general-normalize-hook
							use-package-handler/:ghook)
	:init
	(setq general-override-states
				'(insert
					emacs
					hybrid
					normal
					visual
					motion
					operator
					replace))
	:config
	(general-define-key
	 :states '(normal visual motion emacs)
	 :keymaps 'override
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"
	 "" nil))


;; we descend to hell

(use-package evil :ensure t
	:after general
  :init
  (setq evil-want-keybinding nil
				evil-want-fine-undo t
				evil-want-C-i-jump nil
				evil-want-C-u-scroll t)

	:config
	(general-define-key
	 :states 'normal
	 :prefix "SPC"
	 "fW" 'evil-write-all
	 "w/" '(evil-window-vsplit :which-key "split vertical")
	 "w-" '(evil-window-split :which-key "split horizontal")
	 "wh" 'evil-window-left
	 "wl" 'evil-window-right
	 "wk" 'evil-window-up
	 "wj" 'evil-window-down)

	(setq evil-default-state 'normal)
	(evil-set-initial-state 'dired-mode 'normal)
	(evil-set-initial-state 'message-mode 'motion)

	;; cursor color by state
	(setq evil-insert-state-cursor  '("#268bd2" hbar) ; blue
				evil-normal-state-cursor  '("#b58900" box)  ; blue
				evil-visual-state-cursor  '("#cb4b16" bar)  ; orange
				evil-replace-state-cursor '("#859900" hbar) ; green
				evil-emacs-state-cursor   '("#d33682" box)) ; magenta

	(evil-mode 1))

(use-package evil-matchit :ensure t
	:after evil
	:defines global-evil-matchit-mode
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround :ensure t
	:after evil
	:defines global-evil-surround-mode
  :config
  (global-evil-surround-mode))

(use-package evil-visualstar :ensure t
	:after evil
	:defines global-evil-visualstar-mode
  :config
  (global-evil-visualstar-mode t))


;; abo-abo!

(use-package counsel :ensure t
  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
                "\\|\\.aux\\'"))
	(general-define-key
	 :states '(normal visual insert replace emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 "SPC" 'counsel-M-x
	 "ff" 'counsel-find-file
	 "fl" 'counsel-locate
	 "fr" 'counsel-recentf
	 "g/" '(counsel-git-grep :which-key "git grep")
	 "gf" '(:ignore t :which-key "files")
	 "gff" '(counsel-git :which-key "find git file")
	 "ry" '(counsel-yank-pop :which-key "search kill ring")
	 "hda" '(counsel-apropos :which-key "apropos")))

(use-package recentf
  :defines (recentf-mode)
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-save-file (expand-file-name "~/.recentf")
        recentf-max-saved-items 50)
  (recentf-mode 1))

(use-package ivy :ensure t
	:defines (ivy-mode)
	:config
	(ivy-mode 1)
	(setq ivy-initial-inputs-alist nil ; screw the regex
				ivy-use-virtual-buffers t ; add recentf to `ivy-switch-buffer'
				ivy-height 10
				ivy-count-format "" ; don't count candidates
				;; configure regexp engine to allow out-of-order input
				ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
	(general-define-key
	 :states '(normal emacs)
	 :prefix "SPC"

	 "bb" 'ivy-switch-buffer))

(use-package swiper :ensure t
  :commands 'swiper
	:init
	(general-define-key
	 :states '(normal emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 "/" '(swiper :which-key "swiper find")))


;;; system

(when (fboundp 'winner-mode)
  (winner-mode 1))
(general-define-key
 :states '(normal emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "wu" 'winner-undo
 "wU" 'winner-redo)

(use-package helpful :ensure t
  :commands (helpful-callable
             helpful-function
             helpful-macro
             helpful-command
             helpful-key
             helpful-variable
             helpful-at-point)

  :config
  (general-define-key
   :states '(normal emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "hdf" 'helpful-function
   "hdb" 'describe-bindings
   "hdv" 'helpful-variable
   "hdm" 'helpful-macro
   "hdM" 'describe-mode
   "hdk" 'helpful-key
   "hdK" 'describe-keymap
   "hdc" 'helpful-callable
   "hdC" 'describe-char
   "hdp" 'describe-package))

(use-package pbcopy
  :load-path aero-packages-directory
  :config
  (turn-on-pbcopy))

(use-package tramp
  :config
  ;; From jwiegley: Without this change, tramp ends up sending
  ;; hundreds of shell commands to the remote side to ask what the
  ;; temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.emacs.d/data/tramp"

        ;; my dev server is bsd, which tramp seems to forget
        shell-file-name "/usr/local/bin/bash")

  ;; push projectile in the right direction
	(defadvice projectile-project-root (around ignore-remote first activate)
		(unless (file-remote-p default-directory) ad-do-it))

  (defun aero/tramp-buffer-p (buffer)
		(let ((name (buffer-name buffer)))
			(string-match "^\\*tramp" name)))
	(defun aero/kill-tramp ()
		"Kill and cleanup all Tramp connections. Useful for stale connections."
		(interactive)
		(loop for buffer being the buffers
					do (and (thornjad/tramp-buffer-p buffer) (kill-buffer buffer)))
		(tramp-cleanup-all-connections)))


;;; general bindings

(global-set-key "\C-w" 'backward-kill-word)

(general-define-key
 :states '(normal visual insert replace emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 ;; independent keys
 "TAB" '((switch-to-buffer (other-buffer (current-buffer) 1))
				 :which-key "last buffer")

 "f" '(:ignore t :which-key "files")
 "fw" '(save-buffer :which-key "write buffer")
 "fC" '(:ignore t :which-key "convert")
 "fCd" '(aero/unix2dos :which-key "unix2dos")
 "fCu" '(aero/dos2unix :which-key "dos2unix")
 "fD" '(aero/delete-this-file :which-key "delete this file")
 "fE" '(aero/sudo-edit :which-key "sudo edit")
 "fR" '(aero/rename-this-file-and-buffer :which-key "rename this file")

 "h" '(:ignore t :which-key "help/manual")
 "hd" '(:ignore t :which-key "describe")

 "b" '(:ignore t :which-key "buffers")
 "bn" 'next-buffer
 "bp" 'previous-buffer
 "bl" 'list-buffers
 "bw" 'aero/move-buffer-to-window
 "bm" 'switch-to-messages-buffer
 "bs" 'switch-to-scratch-buffer
 "bd" 'kill-this-buffer
 "bx" 'kill-buffer-and-window

 "a" '(:ignore t :which-key "applications")
 "ad" 'counsel-dired
 "aw" 'browse-url-at-point

 "c" '(:ignore t :which-key "compile")
 "cc" 'compile
 "ck" 'kill-compilation
 "cr" 'recompile

 "e" '(:ignore t :which-key "errors")
 "en" 'next-error
 "ep" 'previous-error

 "F" '(:ignore t :which-key "frame")
 "Fd" 'delete-frame
 "Fo" 'other-frame
 "Ff" 'find-file-other-frame
 "Fn" 'make-frame

 "r" '(:ignore t :which-key "rings")
 "rp" 'aero/clipboard-paste
 "rc" 'aero/clipboard-copy

 "g" '(:ignore t :which-key "git")
 "j" '(:ignore t :which-key "jump")
 "s" '(:ignore t :which-key "sexp")

 "w" '(:ignore t :which-key "window")
 "wb" '(aero/switch-to-minibuffer-window :which-key "switch to minibuffer")
 "wd" 'delete-window
 "wF" 'make-frame
 "wx" 'kill-buffer-and-window
 "w{" 'shrink-window
 "w}" 'enlarge-window)

(provide 'aero-prelude)

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

(require 'cl-lib)


;; garder ma merde à jour

(use-package auto-package-update :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))


;; the general is here

(use-package which-key :ensure t
	:config
	(which-key-mode))
(use-package general :ensure t
	:config
	(general-define-key
	 :states '(normal motion)
	 :prefix "SPC"
	 "" nil))


;; we descend to hell

(use-package evil :ensure t
  :commands (evil-mode)

  :init
  (aero/add-hook!
   'after-init-hook
   (evil-mode 1)
   (setq evil-want-fin-undo t))

	:config

	;; set states for certain modes
	(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode . normal)
																(nrepl-mode . insert)
																(comint-mode . normal)
																(shell-mode . insert)
																(git-commit-mode . insert)
																(git-rebase-mode . normal)
																(term-mode . normal)
																(help-mode . normal)
																(grep-mode . normal)
																(magit-branch-manager-mode . normal)
																(dired-mode . normal)
																(wdired-mode . normal))
				do (evil-set-initial-state mode state))

	(general-define-key
	 :states 'normal
	 :prefix "SPC"
	 "fS" 'evil-write-all))

(use-package evil-matchit :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-visualstar :ensure t
  :config
  (global-evil-visualstar-mode t))

(setq evil-default-state 'normal)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'message-mode 'motion)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'ivy-occur-mode 'emacs)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'esup-mode 'emacs)

;; cursor color by state
(setq evil-insert-state-cursor  '("#268bd2" hbar)  ;; blue
      evil-normal-state-cursor  '("#b58900" box)  ;; blue
      evil-visual-state-cursor  '("#cb4b16" bar)  ;; orange
      evil-replace-state-cursor '("#859900" hbar) ;; green
      evil-emacs-state-cursor   '("#d33682" box)) ;; magenta


;; abo-abo!

(use-package counsel :ensure t
  :config
	(use-package recentf
		:config
		(setq recentf-save-file (expand-file-name "~/.recentf"))
		(recentf-mode 1))
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
	 "gg" '(counsel-git-grep :which-key "git grep")
	 "gf" '(:ignore t :which-key "files")
	 "gff" '(counsel-git :which-key "find git file")
	 "ry" '(counsel-yank-pop :which-key "search kill ring")
	 "hda" '(counsel-apropos :which-kay "apropos")
	 "hdf" 'describe-function
	 "hdb" 'describe-bindings
	 "hdv" 'describe-variable
	 "hdm" 'describe-mode
	 "hdk" 'describe-key
	 "hdK" 'describe-keymap
	 "hdc" 'describe-char
	 "hdp" 'describe-package))

(use-package ivy :ensure t
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
	:config
	(general-define-key
	 :states '(normal emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 "/" '(swiper :which-key "swiper find")))


;; general appearance

(use-package rainbow-delimiters :ensure t
	:config
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package form-feed
	:load-path aero-packages-directory
	:config
	(add-hook 'emacs-lisp-mode-hook 'form-feed-mode))

(use-package beacon :ensure t
	:config (beacon-mode 1))

(use-package dimmer :ensure t
	:config (dimmer-mode))

(when (fboundp 'winner-mode)
  (winner-mode 1))
(general-define-key
 :states '(normal emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "wu" 'winner-undo)


;; general keybindings

;; thanks Steve Yegge!
(global-set-key "\C-w" 'backward-kill-word)

(general-define-key
 :states '(normal visual insert replace emacs)
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

 "g" '(:ignore t :which-key "git")
 "r" '(:ignore t :which-key "rings")
 "w" '(:ignore t :which-key "window"))

(provide 'aero-prelude)

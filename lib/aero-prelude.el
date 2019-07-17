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
(require 'aero-lib)


;; garder ma merde à jour

(use-package auto-package-update
  :load-path "lib/packages/auto-package-update.el/"
	:defines auto-package-update-maybe
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t
        auto-package-update-interval 7 ; days
        auto-package-update-prompt-before-update t)
  (auto-package-update-maybe)

  (defun aero/update-packages ()
    (interactive)
    (auto-package-update-now)))


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
	(setq-default general-override-states
                '(insert
                  hybrid
                  normal
                  visual
                  motion
                  operator
                  replace))
	:config
	(general-define-key
	 :states '(normal visual motion)
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
	 ;; "wh" 'evil-window-left
	 ;; "wl" 'evil-window-right
	 ;; "wk" 'evil-window-up
	 ;; "wj" 'evil-window-down
   "cm" 'evil-make)

  ;; default states
	(setq evil-default-state 'normal)
	(evil-set-initial-state 'dired-mode 'emacs)
	(evil-set-initial-state 'message-mode 'motion)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)

  (defun aero/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (defun aero/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (evil-define-key 'visual global-map (kbd ">") 'aero/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'aero/evil-shift-left)

	(evil-mode 1))

(use-package evil-matchit :ensure t
	:after evil
	:defines global-evil-matchit-mode
  :config
  (global-evil-matchit-mode 1))

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
	 :states '(normal visual insert replace)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 "SPC" 'counsel-M-x
	 "ff" 'counsel-find-file
	 "fl" 'counsel-locate
	 ;; "fr" 'counsel-recentf
	 "g/" '(counsel-git-grep :which-key "git grep")
	 "gf" '(:ignore t :which-key "files")
	 "gff" '(counsel-git :which-key "find git file")
	 "ry" '(counsel-yank-pop :which-key "search kill ring")
	 "hda" '(counsel-apropos :which-key "apropos")
   "u" 'counsel-unicode-char))

(use-package recentf
  :defines (recentf-mode)
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-save-file (expand-file-name "~/.recentf")
        recentf-max-saved-items 500
        ;; never cleanup, this will get rid of tramp files
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package ivy :ensure t
  :functions ivy-mode
	:config
	(ivy-mode 1)
	(setq ivy-initial-inputs-alist nil ; screw the regex
				ivy-use-virtual-buffers t ; add recentf to `ivy-switch-buffer'
				ivy-height 10
				ivy-count-format "" ; don't count candidates
				;; configure regexp engine to allow out-of-order input
				ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
	(general-define-key
	 :states '(normal)
	 :prefix "SPC"

	 "bb" 'ivy-switch-buffer))

(use-package swiper :ensure t
  :commands swiper
	:init
	(general-define-key
	 :states '(normal)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 "/" '(counsel-grep-or-swiper :which-key "search")))


;;; system

(use-package winner
  :defines winner-boring-buffers
  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*esh command on file*"))
  (winner-mode 1)
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "wu" 'winner-undo
   "wU" 'winner-redo))

;; windmove
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-j") #'windmove-down)
(global-set-key (kbd "M-k") #'windmove-up)
(global-set-key (kbd "M-l") #'windmove-right)

(use-package helpful :ensure t
  :after evil
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "hdf" 'helpful-function
   "hdF" 'counsel-describe-face
   "hdb" 'describe-bindings
   "hdv" 'helpful-variable
   "hdm" 'helpful-macro
   "hdM" 'describe-mode
   "hdk" 'helpful-key
   "hdK" 'describe-keymap
   "hdc" 'helpful-callable
   "hdC" 'describe-char
   "hdp" 'describe-package)

  (require 'evil)
  (evil-define-key 'normal helpful-mode-map
    "q" 'kill-this-buffer
    "?" 'describe-mode))

(use-package howdoi :ensure t
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "hs" '(howdoi-query :which-key "howdoi")
   "hl" '(howdoi-query-line-at-point :which-key "howdoi line")
   ))

(use-package pbcopier
  :load-path "lib/packages/pbcopier/"
  :config
  (turn-on-pbcopier))

(use-package re-builder
	:config
	(setq reb-re-syntax 'string))

(use-package tramp
  :defer t
  :functions tramp-cleanup-all-connection
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
		(cl-loop for buffer being the buffers
					do (and (aero/tramp-buffer-p buffer) (kill-buffer buffer)))
		(tramp-cleanup-all-connections)))

(use-package quick-restart
  :load-path "lib/packages/quick-restart/")

(use-package ranger :ensure t
  :config
  (setq ranger-show-hidden t
        find-directory-functions 'deer)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fd" 'deer))

(use-package pomp
  :load-path "lib/packages/pomp/"
  :commands pomp
  :init
  (evil-set-initial-state 'pomp-mode 'emacs)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "ap" 'pomp))


;;; general bindings

(global-set-key (kbd "C-w") 'aero/smarter-backward-kill-word)

(general-define-key
 :states '(normal insert motion)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "" nil

 ;; independent keys
 "TAB" '(aero/alternate-buffer :which-key "alternate buffer")
 "q" 'quoted-insert
 "'" 'eshell
 "\"" '(aero/eshell-new :which-key "eshell-new")
 ":" 'eval-expression
 ";" 'comment-or-uncomment-region
 "!" 'shell-command

 "U" 'universal-argument

 "f" '(:ignore t :which-key "files")
 "fw" '(save-buffer :which-key "write buffer")
 "fC" '(:ignore t :which-key "convert")
 "fCd" '(aero/unix2dos :which-key "unix2dos")
 "fCu" '(aero/dos2unix :which-key "dos2unix")
 "fD" '(aero/delete-this-file :which-key "delete this file")
 "fE" '(aero/sudo-edit :which-key "sudo edit")
 "fR" '(aero/rename-this-file-and-buffer :which-key "rename this file")
 "fo" '(:ignore t :which-key "open special files")
 "fod" '(aero/open-tweaks :which-key "tweaks")
 "fot" '(aero/thornlog :which-key "thornlog")
 "foD" '(aero/reload-tweaks :which-key "reload tweaks")
 "ft" 'untabify-buffer
 "fT" 'tabify-buffer
 "fi" 'indent-buffer

 "h" '(:ignore t :which-key "help/manual")
 "hm" 'woman
 "hi" 'info
 "hI" 'info-apropos
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

 "e" '(:ignore t :which-key "emacs")
 "eq" 'aero/save-kill-emacs
 "ea" 'aero/apologize-to-emacs

 "a" '(:ignore t :which-key "applications")
 "ad" 'counsel-dired

 "c" '(:ignore t :which-key "compile")
 "cc" 'compile
 "cC" 'byte-compile-file
 "cD" 'byte-recompile-directory
 "ck" 'kill-compilation
 "cr" 'recompile
 "cR" 'byte-recompile-file
 "ce" '(:ignore t :which-key "eval")
 "cer" 'eval-region
 "ceb" 'eval-buffer
 "ced" 'eval-defun

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
 "m" '(:ignore t :which-key "mode")
 "p" '(:ignore t :which-key "project")

 "w" '(:ignore t :which-key "window/web")
 "wb" '(aero/switch-to-minibuffer-window :which-key "switch to minibuffer")
 "wd" 'delete-window
 "wF" 'make-frame
 "wx" 'kill-buffer-and-window
 "w{" 'shrink-window
 "w}" 'enlarge-window)

(provide 'aero-prelude)

;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
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

(require 'cl-lib)
(require 'straight)
(require 'use-package)
(require 'aero-lib)

;;; Code:

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(declare-function straight-use-package "straight.el")
(straight-use-package 'org)


;;; Set up core packages
(use-package use-package-ensure-system-package :straight t)
(use-package gnu-elpa-keyring-update :straight t)
(use-package exec-path-from-shell :straight t
  :config
  (when (or (memq window-system '(mac ns x)) (daemonp))
    (exec-path-from-shell-initialize)))


;;; get ready to patch at any time

(use-package el-patch :straight t
  :init
  (setq el-patch-enable-use-package-integration t))
;; Only needed at compile time
(eval-when-compile
  (require 'el-patch))


;;; used in several places

(use-package ripgrep :straight t)


;;; the general is here

(use-package general
  :init
  (setq-default general-override-states
                '(insert hybrid normal visual motion operator replace))
  :config
	(general-create-definer aero-leader-def
		:states '(normal visual emacs motion)
		:prefix "SPC"
		:non-normal-prefix "C-SPC")
	(general-create-definer aero-mode-leader-def
		:states '(normal visual emacs motion)
		:prefix ",")

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil)

	;; Main configuration

  (general-def
    ;; Emacs chose ^? for the help system for some despicable reason. Fuck that.
    (kbd "C-h") 'delete-backward-char
    (kbd "C-w") 'aero/smarter-backward-kill-word
    (kbd "C-TAB") 'insert-tab
    (kbd "M-TAB") 'aero/alternate-buffer)

  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fW" 'evil-write-all
   "w/" '(evil-window-vsplit :wk "split vertical")
   "w-" '(evil-window-split :wk "split horizontal")
   "cm" 'evil-make)

  (global-set-key [remap keyboard-quit] #'aero/keyboard-quit-context)

  ;; mode-specific overrides
  (general-define-key
   :states '(normal insert motion)
   :keymaps 'override
   :prefix ","
   "" nil)

  (general-define-key
   :states '(normal insert motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil

   ;; independent keys
   "TAB" '(aero/alternate-buffer :wk "alternate buffer")
   (kbd "ESC") 'keyboard-quit
   (kbd "C-g") 'keyboard-quit
   "'" 'eshell
   "\"" '(aero/eshell-new :wk "eshell-new")
   ":" 'eval-expression
   ";" 'comment-or-uncomment-region
   "!" 'shell-command
   "=" 'quick-calc

   ;; NOTE deprecated, use the top-level "," prefix instead
   "," '(:ignore t :wk "mode") ; reserved for mode-specific

   "U" 'universal-argument
   "z" 'repeat

   "q" '(:ignore t :wk "quoted insert")
   "qq" 'quoted-insert
   "ql" 'insert-lambda

   "f" '(:ignore t :wk "files")
   "fw" '(save-buffer :wk "write buffer")
   "fW" '(aero/async-write-buffer :wk "async write buffer (experimental)")
   "fC" '(:ignore t :wk "convert")
   "fCd" '(aero/unix2dos :wk "unix2dos")
   "fCu" '(aero/dos2unix :wk "dos2unix")
   "fD" '(aero/delete-this-file :wk "delete this file")
   "fE" '(aero/sudo-edit :wk "sudo edit")
   "fR" '(aero/rename-this-file-and-buffer :wk "rename this file")
   "fx" '(aero/xdg-open :wk "xdg-open")
   "fo" '(:ignore t :wk "open special files")
   "fot" '(:ignore t :wk "thornlog")
   "fott" '(aero/thornlog-dir :wk "thornlog all")
   "fotl" '(aero/thornlog-log :wk "thornlog log")
   "fotd" '(aero/thornlog-todo :wk "thornlog todo")
   "foP" 'aero/open-emacs-problems
   "fof" 'aero/open-local-init

   "o" '(:ignore t :wk "org")
   "oa" 'org-agenda
   "ot" 'aero/task
   "oe" '(:ignore t :wk "org edit")
   "oet" '(:ignore t :wk "org table")
   "oets" 'org-table-sort-lines

   "h" '(:ignore t :wk "help/manual")
   "hM" 'woman
   "hm" 'man
   "hi" 'info
   "hI" 'info-apropos
   "hd" '(:ignore t :wk "describe")
   "hw" '(:ignore t :wk "which-key")
   "hwm" '(which-key-show-major-mode :wk "major mode map")

   "b" '(:ignore t :wk "buffers")
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "bl" 'ibuffer
   "bL" 'list-buffers
   "bm" 'switch-to-messages-buffer
   "br" '(aero/reopen-file-at-buffer :wk "reopen file")
   "bs" 'switch-to-scratch-buffer
   "bS" 'switch-to-new-scratch-buffer
   "bd" 'kill-this-buffer
   "bx" 'kill-buffer-and-window
   "bw" '(whitespace-mode :wk "whitespace")
   "bt" '(:ignore t :wk "tabify")
   "btu" 'untabify-buffer
   "btt" 'tabify-buffer
   "bi" 'indent-buffer
   "bP" 'aero/toggle-prettify-this-buffer

   "E" '(:ignore t :wk "emacs")
   "Et" 'counsel-load-theme
   "Ea" 'aero/apologize-to-emacs
   "Ed" '(:ignore t :wk "debug")
   "Ede" 'toggle-debug-on-error
   "Edq" 'toggle-debug-on-quit

   "a" '(:ignore t :wk "applications")
   "ad" 'counsel-dired
   "ag" '(:ignore t :wk "games")
   "agt" 'tetris
   "agd" 'dunnet

   "c" '(:ignore t :wk "compile")
   "cc" 'compile
   "cC" '(aero/byte-recompile-file-at-buffer :wk "byte recompile file at buffer")
   "ck" 'kill-compilation
   "cr" 'recompile
   "cR" 'byte-recompile-file
   "ce" '(:ignore t :wk "elisp")
   "cei" '(ielm :wk "ielm repl")
   "cer" 'eval-region
   "ceb" 'eval-buffer
   "ced" 'eval-defun
   "cec" '(:ignore t :wk "byte compile")
   "cecb" '(aero/byte-compile-file-at-buffer :wk "file at buffer")
   "cecr" '(aero/byte-recompile-file-at-buffer :wk "file at buffer (recompile)")
   "cecf" '(byte-compile-file :wk "other file")
   "cecF" '(async-byte-compile-file :wk "other file async")
   "cecd" '(byte-recompile-directory :wk "directory")

   "e" '(:ignore t :wk "errors")
   "en" 'next-error
   "ep" 'previous-error

   "F" '(:ignore t :wk "frame")
   "Fd" 'delete-frame
   "Fo" 'other-frame
   "FF" 'find-file-other-frame
   "Fn" 'make-frame
   "Fm" 'toggle-frame-maximized
   "Ff" 'toggle-frame-fullscreen

   "g" '(:ignore t :wk "git")
   "gf" '(:ignore t :wk "files")

   "l" '(:ignore t :wk "lsp")

   "L" '(:ignore t :wk "local")

   "j" '(:ignore t :wk "jump")
   "s" '(:ignore t :wk "sexp")
   "m" '(:ignore t :wk "mode")
   "d" '(:ignore t :wk "debug")

   "p" '(:ignore t :wk "project")
   "pr" '(xref-find-definitions :wk "find ref")

   "P" '(:ignore t :wk "packages/perspective")
   "PP" '(:ignore t :wk "packages")
   "PPp" 'straight-pull-package-and-deps
   "PPF" 'straight-fetch-all
   "PPP" 'straight-pull-all
   "PPg" 'straight-get-recipe
   "PPC" 'straight-check-all
   "PPr" 'straight-rebuild-package
   "PPR" 'straight-rebuild-all
   "PPx" 'straight-prune-build

   "u" 'undo-tree-visualize

   "S" '(:ignore t :wk "shell/sql")
   "Se" 'eshell
   "SE" '(:ignore t :wk "eshell")
   "St" '(:ignore t :wk "term")

   "w" '(:ignore t :wk "window/web")
   "w=" 'balance-windows
   "wB" '(aero/switch-to-minibuffer-window :wk "switch to minibuffer")
   "wd" 'delete-window
   "wF" 'make-frame
   "wx" 'kill-buffer-and-window
   "w{" 'shrink-window
   "w}" 'enlarge-window
   "wm" 'maximize-window
   "wi" 'minimize-window
   "wo" 'browse-url-xdg-open

   "wL" '(:ignore t :wk "layout")
   "wL2" 'aero/layout-two-columns
   "wL3" 'aero/layout-three-columns

   "t" '(:ignore t :wk "tabs/text")
   "tU" 'upcase-dwim
   "tD" 'downcase-dwim
   "ts" 'sort-lines
   "tn" '(:ignore t :wk "number")
   "tni" 'increment-number-at-point
   "tnd" 'decrement-number-at-point
   )
	)

(use-package which-key :straight t
  :defines which-key-mode
  :config
  (which-key-mode)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")))


;; we descend to hell

(use-package evil :straight t
  :init
  (setq evil-want-keybinding nil ; handled by evil-collection
        ;; to change undo-system without restart, use SPC-: `evil-set-undo-system'
        evil-undo-system 'undo-tree
        evil-want-fine-undo t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t)

  :config
  (define-key evil-motion-state-map " " nil)

  ;; default states
  (setq evil-default-state 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'message-mode 'motion)

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
  ;; :q should kill the current buffer rather than quitting emacs entirely
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  ;; Need to type out :quit to close emacs
  (evil-ex-define-cmd "quit" 'evil-quit)

  (evil-define-key 'normal 'global
    ;; Run macro in register q
    "Q" "@q")
  (evil-define-key 'visual 'global
    ;; run macro in register q on region
    "Q" (kbd ":norm @q RET")
    ;; repeat on region
    "." (kbd ":norm . RET"))

  (evil-mode 1))

(use-package evil-matchit :straight t :defer 1
  :after evil
  :defines global-evil-matchit-mode
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar :straight t
  :after evil
  :defines global-evil-visualstar-mode

  :config
  (global-evil-visualstar-mode t))


;; abo-abo!

(use-package counsel :straight t
  :after general

  :init/el-patch ; remove bindings which we use `helpful' for
  (defvar counsel-mode-map
    (let ((map (make-sparse-keymap)))
      (dolist (binding
               '((execute-extended-command . counsel-M-x)
                 (describe-bindings . counsel-descbinds)
                 (el-patch-remove
                   (describe-function . counsel-describe-function)
                   (describe-variable . counsel-describe-variable))
                 (apropos-command . counsel-apropos)
                 (describe-face . counsel-describe-face)
                 (list-faces-display . counsel-faces)
                 (find-file . counsel-find-file)
                 (find-library . counsel-find-library)
                 (imenu . counsel-imenu)
                 (load-library . counsel-load-library)
                 (load-theme . counsel-load-theme)
                 (yank-pop . counsel-yank-pop)
                 (info-lookup-symbol . counsel-info-lookup-symbol)
                 (pop-to-mark-command . counsel-mark-ring)
                 (bookmark-jump . counsel-bookmark)))
        (define-key map (vector 'remap (car binding)) (cdr binding)))
      map)
    (el-patch-concat
      "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements."
      (el-patch-add
        "\n\nBindings that are remapped by `helpful' have been removed.")))

  (defcustom counsel-mode-override-describe-bindings nil
    "Whether to override `describe-bindings' when `counsel-mode' is active."
    :type 'boolean
    :group 'counsel)

  (define-minor-mode counsel-mode
    "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel mode remaps
built-in emacs functions that have counsel replacements.
Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}"
    :global t
    :keymap counsel-mode-map
    :group 'counsel
    (if counsel-mode
        (progn
          (when (and (fboundp 'advice-add)
                     counsel-mode-override-describe-bindings)
            (advice-add #'describe-bindings :override #'counsel-descbinds))
          (define-key minibuffer-local-map (kbd "C-r")
            'counsel-minibuffer-history))
      (when (fboundp 'advice-remove)
        (advice-remove #'describe-bindings #'counsel-descbinds))))

  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
                "\\|\\.aux\\'"))

  (defun aero/counsel-unicode-char-after ()
    "Like `counsel-unicode-char', but insert after point"
    (interactive)
    (save-excursion
      (right-char)
      (counsel-unicode-char)))

  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-git-cmd "rg --files"
        counsel-rg-base-command "rg --with-filename --smart-case --no-heading --line-number --color never %s")

  (aero-leader-def
    "SPC" 'counsel-M-x
    "ff" 'counsel-find-file
    "fl" 'counsel-locate
    "fr" 'counsel-recentf
    "?" 'counsel-rg
    "gg" '(counsel-git-grep :wk "git grep")
    "gff" '(counsel-git :wk "find git file")
    "ry" '(counsel-yank-pop :wk "search kill ring")
    "hda" '(counsel-apropos :wk "apropos")
    "qu" '(aero/counsel-unicode-char-after :wk "unicode char")
    "qU" 'counsel-unicode-char))

(use-package recentf
  :defines (recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name "recentf" aero-etc-dir)
        recentf-max-saved-items 500
        ;; never cleanup, this will get rid of tramp files
        recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package ivy :straight t
  :after general
  :functions ivy-mode
  :config
  (use-package flx :straight t)

  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil ; screw the regex
        ivy-use-virtual-buffers t ; add recentf to `ivy-switch-buffer'
        ivy-virtual-abbreviate 'full
        ivy-wrap t
        ivy-height 8
        ivy-count-format "" ; don't count candidates

        ;; use fuzzy by default, but some searching is impossible without
        ;; stricter regex's
        ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (counsel-git-grep . ivy--regex-plus)
                                (projectile-ripgrep . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-plus)))
  (general-define-key
   :states '(normal)
   :prefix "SPC"

   "bb" 'ivy-switch-buffer)

  (use-package ivy-rich :straight t
    :config
    (ivy-rich-mode 1)))

(use-package swiper :straight t
  :after general
  :commands swiper
  :init
  (aero-leader-def
    "/" '(counsel-grep-or-swiper :wk "search")
    "?" '(swiper-thing-at-point :wk "search thing at point")))

(use-package avy :straight t
  :commands (avy-goto-line avy-goto-char avy-goto-word-1)
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jl" '(avy-goto-line :wk "jump to line")
   "jc" '(avy-goto-char :wk "jump to char")
   "jw" '(avy-goto-word-1 :wk "jump to word")))

(use-package dumb-jump :straight t
  :init
  (setq dumb-jump-prefer-searcher 'rg)
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jj" '(dumb-jump-go :wk "go")))


;;; system

(use-package undo-tree :straight t
  :config
  (global-undo-tree-mode +1)

  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (setq undo-tree-enable-undo-in-region nil))

(use-package winner
  :after general
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
   "wU" 'winner-redo
   "wh" 'windmove-left
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right))

(use-package winum :straight t
  :init
  (winum-mode)
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "1" '(winum-select-window-1 :wk "window-1")
   "2" '(winum-select-window-2 :wk "window-2")
   "3" '(winum-select-window-3 :wk "window-3")
   "4" '(winum-select-window-4 :wk "window-4")
   "5" '(winum-select-window-5 :wk "window-5")
   "6" '(winum-select-window-6 :wk "window-6")
   "7" '(winum-select-window-7 :wk "window-7")
   "8" '(winum-select-window-8 :wk "window-8")
   "9" '(winum-select-window-9 :wk "window-9")
   "wg" '(winum-select-window-by-number :wk "select window by number"))

  ;; collapse all those window commands to one summary
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
        which-key-replacement-alist)
  (push '((nil . "select-window-[1-9]") . t) which-key-replacement-alist))

;; windmove
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-j") #'windmove-down)
(global-set-key (kbd "M-k") #'windmove-up)
(global-set-key (kbd "M-l") #'windmove-right)

(use-package neotree :straight t
  :after winum
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :init
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point t
        neo-autorefresh t
        neo-mode-line-type 'none
        neo-window-width 35
        neo-show-updir-line t
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files t
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$"))

  :config
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

  (aero-leader-def
    "0" 'neotree-toggle
    ")" 'neotree-project-dir)

  ;; Fix for evil
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(use-package helpful :straight t
  :after (evil general)
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

(when (system-is-mac)
  (use-package pbcopier
    :functions (turn-on-pbcopier)
    :straight (:host gitlab :repo "thornjad/pbcopier")
    :config (turn-on-pbcopier)))

(when (system-is-linux)
  (setq select-enable-clipboard t)
  (setq interprogram-paste-function #'gui-selection-value))

(use-package re-builder
  :commands re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package tramp
  :straight (tramp :host nil :repo "git://git.savannah.gnu.org/tramp.git")
  :defer t
  :functions tramp-cleanup-all-connection
  :config
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.config/emacs/data/tramp"
        tramp-default-method "rsync"
        tramp-terminal-type "tramp")

  ;; push projectile in the right direction
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

(use-package ranger :straight t
  :after general
  :config
  (setq ranger-show-hidden t
        find-directory-functions 'deer)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fd" 'deer))

(use-package pomp
  :straight (:host gitlab :repo "thornjad/pomp")
  :after general
  :commands pomp
  :init
  (evil-set-initial-state 'pomp-mode 'emacs)
  (global-set-key (kbd "<f12>") 'pomp)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "ap" 'pomp))

(use-package expand-region :straight t
  :defer nil
  :config
  (general-define-key
   :states '(normal visual motion replace emacs)
   :keymaps 'override
   (kbd "C-e") 'er/expand-region))


;;; General crap

(use-package powerthesaurus :straight t
  :commands powerthesaurus-lookup-word-dwim
  :init
  (aero-leader-def
    "tt" '(powerthesaurus-lookup-word-dwim :wk "thesaurus")))

;; Ensure emacsclient frames open with focus
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))


(provide 'aero-prelude)

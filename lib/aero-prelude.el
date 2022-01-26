;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
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
;;

(require 'cl-lib)
(require 'straight)
(require 'use-package)
(require 'aero-lib)

;;; Code:


;;; Set up core packages
(use-package gnu-elpa-keyring-update :straight t)
(use-package exec-path-from-shell :straight t :defer 1
  :config
  (when (or (window-system) (daemonp))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

(use-package no-littering :defer 5
  :after recentf
  :straight (:host github :repo "emacscollective/no-littering")
  :defines (no-littering-var-directory
            no-littering-etc-directory)
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


;;; get ready to patch at any time

(use-package el-patch :straight t
  :init (setq el-patch-enable-use-package-integration t))
;; Only needed at compile time
(eval-when-compile
  (require 'el-patch))


;;; used in several places

(use-package ripgrep :straight t :defer 3)


;;; the general is here

(use-package general :straight t
  :functions (general-define-key)
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
		:prefix "SPC ,")

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
   "fotl" '(aero/thornlog-today :wk "thornlog log")
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

   "m" '(tmm-menubar :wk "Context menu")

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
   "cec" '(:ignore t :wk "compile")
   "cecb" '(aero/native-compile-file-at-buffer :wk "file at buffer")
   "ceB" '(:ignore t :wk "byte compile")
   "ceBb" '(aero/byte-compile-file-at-buffer :wk "file at buffer")
   "ceBr" '(aero/byte-recompile-file-at-buffer :wk "file at buffer (recompile)")
   "ceBf" '(byte-compile-file :wk "other file")
   "ceBF" '(async-byte-compile-file :wk "other file async")
   "ceBd" '(byte-recompile-directory :wk "directory")

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

   "T" '(:ignore t :wk "tab")
   "TT" 'tab-bar-mode
   "Tj" 'tab-next
   "Tk" 'tab-previous
   "Tc" '(tab-new :wk "create tab")
   "TL" 'tab-list
   "TL" 'tab-last
   "T TAB" 'tab-recent
   "T," 'tab-rename
   "Ts" '(tab-duplicate :wk "tab duplicate split")
   "Td" 'tab-close
   "Tu" 'tab-undo
   "Tg" '(tab-select :wk "tab go")
   "Tb" 'switch-to-buffer-other-tab
   "Tf" 'find-file-other-tab

   "w" '(:ignore t :wk "window/web")
   "ww" 'eww
   "wp" 'browse-url-at-point
   "ws" '(eww-search-words :which-key "web search")
   "w=" 'balance-windows
   "wB" '(aero/switch-to-minibuffer-window :wk "switch to minibuffer")
   "wd" 'delete-window
   "wF" 'make-frame
   "w{" 'shrink-window
   "w}" 'enlarge-window
   "wm" 'maximize-window
   "wi" 'minimize-window
   "wo" 'browse-url-xdg-open
   "wb" '(:ignore t :wk "browse")
   "wbm" 'eww-list-bookmarks
   "wbb" 'eww-list-buffers
   "wbh" 'eww-list-histories

   "wL" '(:ignore t :wk "layout")
   "wL2" 'aero/layout-two-columns
   "wL3" 'aero/layout-three-columns

   "t" '(:ignore t :wk "tabs/text")
   "tf" 'fill-paragraph
   "tU" 'upcase-dwim
   "tD" 'downcase-dwim
   "ts" 'sort-lines
   "tn" '(:ignore t :wk "number")
   "tni" 'increment-number-at-point
   "tnd" 'decrement-number-at-point))

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
  (use-package evil-collection
    :config (evil-collection-init))

  (define-key evil-motion-state-map " " nil)

  ;; default states
  (setq evil-default-state 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'message-mode 'motion)

  ;; Make movement keys work like they should by remapping next to next-visual, etc.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  ;; Ensure horizontal movement doesn't cross lines
  (setq-default evil-cross-lines nil)

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
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-e") #'evil-backward-word-end)
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-M-e") #'evil-backward-WORD-end)
  (evil-define-key '(insert) global-map
    (kbd "C-y") #'evil-paste-after)
  (evil-define-key '(insert) global-map
    (kbd "C-S-y") #'evil-paste-before)

  (evil-define-key 'normal 'global
    ;; Run macro in register q
    "Q" "@q")
  (evil-define-key 'visual 'global
    ;; run macro in register q on region
    "Q" (kbd ":norm @q RET")
    ;; repeat on region
    "." (kbd ":norm . RET"))

  (evil-mode 1))

;; Doesn't do anything for GUI, so don't bother
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :after evil
    :functions (evil-terminal-cursor-changer-activate)
    :config (evil-terminal-cursor-changer-activate)))


(use-package evil-matchit :straight t :defer 5
  :after evil
  :defines global-evil-matchit-mode
  :config (global-evil-matchit-mode 1))

(use-package evil-visualstar :straight t :defer 5
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
  (defvar aero-etc-dir)
  (setq recentf-save-file (expand-file-name "recentf" aero-etc-dir)
        recentf-max-saved-items 500)
  (recentf-mode 1)

  ;; run recentf save every 5 minutes
  (run-at-time 60 (* 5 60) 'recentf-save-list))

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
        ivy-height 12
        ivy-fixed-height-minibuffer t
        ;; ne pas quitter le minibuffer en cas de `delete-error`
        ivy-on-del-error-function #'ignore
        ;; don't bother counting candidates
        ivy-count-format ""
        ;; use fuzzy by default, but some searching is impossible without
        ;; stricter regex's
        ivy-re-builders-alist '((t . ivy--regex-plus)))
  (aero-leader-def
    "bb" 'ivy-switch-buffer))

;; Currently disabled but preserved because I can't decide whether I want it or not. Using it means
;; the mode line doesn't jump up on every M-x but also takes some character away from Emacs.
(use-package ivy-posframe :straight t :disabled t
  :after (ivy)
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))

  ;; Fix atrocious width jumping
  (defun aero/ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (setq ivy-posframe-size-function 'aero/ivy-posframe-get-size)

  (ivy-posframe-mode +1))

(use-package ivy-rich :straight t
  :after (counsel ivy)
  :defines (ivy-rich-path-style)
  :functions (ivy-rich-mode)
  :config
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))

(use-package all-the-icons :straight t)
(use-package all-the-icons-ivy-rich :straight t
  :after (all-the-icons ivy-rich)
  :functions (all-the-icons-ivy-rich-mode)
  :init (all-the-icons-ivy-rich-mode +1))

(use-package swiper :straight t
  :after (general counsel)
  :commands (swiper counsel-grep-or-swiper swiper-thing-at-point)
  :init
  (aero-leader-def
    "/" '(counsel-grep-or-swiper :wk "search")
    "?" '(swiper-thing-at-point :wk "search thing at point"))
  :config
  (setq swiper-action-recenter t))

(use-package avy :straight t
  :commands (avy-goto-line avy-goto-char avy-goto-word-1)
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jl" '(avy-goto-line :wk "jump to line")
   "jc" '(avy-goto-char :wk "jump to char")
   "jw" '(avy-goto-word-1 :wk "jump to word")))

(use-package ace-link :straight (:host github :repo "abo-abo/ace-link")
  :after (avy)
  :functions (ace-link-setup-default)
  :init (ace-link-setup-default))


;;; system

(use-package undo-tree :straight t
  :config
  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-auto-save-history t)

  (global-undo-tree-mode +1))

(use-package winner
  :after (general)
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
  (declare-function general-define-key "general.el")
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "wu" 'winner-undo
   "wU" 'winner-redo
   "wh" 'windmove-left
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right)

  ;; windmove
  (global-set-key (kbd "M-h") #'windmove-left)
  (global-set-key (kbd "M-j") #'windmove-down)
  (global-set-key (kbd "M-k") #'windmove-up)
  (global-set-key (kbd "M-l") #'windmove-right))

(use-package winum :straight t :defer 5
  :after (general)
  :init
  (winum-mode)
  :config
  (aero-leader-def
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

(use-package helpful :straight t
  :commands (helpful-function
             helpful-variable
             helpful-macro
             helpful-key
             helpful-callable)
  :after (evil general)
  :init
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

  :config
  (evil-define-key 'normal helpful-mode-map
    "q" 'kill-this-buffer
    "?" 'describe-mode))

;;; System-specifics

;; Mac needs some extra hand-holding to connect the kill-ring to the system
;; clipboard.
(when (system-is-mac)
  (declare-function aero/pbcopier-select-text "aero-lib.el")
  (declare-function aero/pbcopier-selection-value "aero-lib.el")
	(setq interprogram-cut-function #'aero/pbcopier-select-text)
	(setq interprogram-paste-function #'aero/pbcopier-selection-value)

  (setq-default ns-use-native-fullscreen nil)
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF")))

(when (system-is-linux)
  (setq select-enable-clipboard t
        interprogram-paste-function #'gui-selection-value
        x-gtk-use-system-tooltips t
        dired-listing-switches "-lFaGh1v --group-directories-first"))

;; TTY also needs some clipboard help. Only works in certain term emulators,
;; Alacritty is my preferred.
(unless (display-graphic-p)
  (use-package xclip :straight t
    :config (xclip-mode +1)))


;; File navigation

(use-package tramp
  :straight (tramp :host nil :repo "git://git.savannah.gnu.org/tramp.git")
  :defer t
  :functions tramp-cleanup-all-connection
  :config
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.config/emacs/data/tramp"
        tramp-use-ssh-controlmaster-options nil  ; use system settings instead
        tramp-default-method "rsync"
        tramp-terminal-type "tramp")

  ;; push projectile in the right direction
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it)))

(use-package ranger :straight t
  :commands (deer)
  :after general
  :init
  (setq ranger-show-hidden t
        find-directory-functions 'deer)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "fd" 'deer))


;;; General crap

(use-package pomp
  :straight (:host gitlab :repo "thornjad/pomp")
  :after (general evil)
  :commands (pomp)
  :init
  (evil-set-initial-state 'pomp-mode 'emacs)
  (global-set-key (kbd "<f12>") 'pomp)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "ap" 'pomp))

(use-package editorconfig :straight t :defer 5
  :functions (editorconfig-mode)
  :config (editorconfig-mode +1))

;; Ensure emacsclient frames open with focus
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))


(provide 'aero-prelude)

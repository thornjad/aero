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
;;; Commentary:
;;
;; After init.el, this file is the core driver of Aero. It sets up ubiquitous packages and the
;; primary keybindings.
;;
;; The file name of "prelude" has nothing to do with the excellent Emacs configuration distribution
;; of the same name. Instead, the name alludes to the fact that the configuration here comes before
;; the rest, and in many cases is required by other packages.

(require 'cl-lib)
(require 'straight)
(require 'use-package)
(require 'aero-lib)

;;; Code:


;; Set up core packages. The ELPA keyring sometimes gets screwed up, but this package lets us fix
;; it easily.
(use-package gnu-elpa-keyring-update :straight t)

;; Mostly only required for MacOS, we need to grab environment variables from the default shell.
;; This lets us use TRAMP more easily and connects us with some tools.
(use-package exec-path-from-shell :straight t :defer 1
  :config
  (when (or (window-system) (daemonp))
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "PATH"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;; Faster than grep, but requires ripgrep to be installed locally
(use-package ripgrep :straight t :defer 3)


;;; the general is here

;; General lets us more easily set keybindings throughout Aero
(use-package general :straight t
  :functions (general-define-key)
  :init
  (setq-default general-override-states
                '(insert hybrid normal visual motion operator replace))
  :config

  ;; Most bindings will fall under this leader key, so we make a handy macro.
	(general-create-definer aero-leader-def
		:states '(normal visual emacs motion)
		:prefix "SPC"
		:non-normal-prefix "C-SPC")

  ;; Mode-leader lets us put keybindings only in specific modes (usually major modes).
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
   "w2" 'aero/layout-two-columns
   "w3" 'aero/layout-three-columns
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

   "C" '(:ignore t :wk "clue")

   "E" '(:ignore t :wk "emacs")
   "Ed" '(:ignore t :wk "debug")
   "Ede" 'toggle-debug-on-error
   "Edq" 'toggle-debug-on-quit
   "Et" 'counsel-load-theme

   "F" '(:ignore t :wk "frame")
   "FF" 'find-file-other-frame
   "Fd" 'delete-frame
   "Ff" 'toggle-frame-fullscreen
   "Fm" 'toggle-frame-maximized
   "Fn" 'make-frame
   "Fo" 'other-frame

   "L" '(:ignore t :wk "local")

   "P" '(:ignore t :wk "packages/perspective")
   "PP" '(:ignore t :wk "packages")
   "PPC" 'straight-check-all
   "PPF" 'straight-fetch-all
   "PPP" 'straight-pull-all
   "PPR" 'straight-rebuild-all
   "PPg" 'straight-get-recipe
   "PPp" 'straight-pull-package-and-deps
   "PPr" 'straight-rebuild-package
   "PPx" 'straight-prune-build

   "S" '(:ignore t :wk "shell/sql")
   "SE" '(:ignore t :wk "eshell")
   "Se" 'eshell
   "St" '(:ignore t :wk "term")

   "T TAB" 'tab-recent
   "T" '(:ignore t :wk "tab")
   "T," 'tab-rename
   "TL" 'tab-last
   "TL" 'tab-list
   "TT" 'tab-bar-mode
   "Tb" 'switch-to-buffer-other-tab
   "Tc" '(tab-new :wk "create tab")
   "Td" 'tab-close
   "Tf" 'find-file-other-tab
   "Tg" '(tab-select :wk "tab go")
   "Tj" 'tab-next
   "Tk" 'tab-previous
   "Ts" '(tab-duplicate :wk "tab duplicate split")
   "Tu" 'tab-undo

   "U" 'universal-argument

   "a" '(:ignore t :wk "applications")
   "ad" 'counsel-dired
   "ag" '(:ignore t :wk "games")
   "agd" 'dunnet
   "agt" 'tetris

   "b" '(:ignore t :wk "buffers")
   "bL" 'list-buffers
   "bS" 'switch-to-new-scratch-buffer
   "bd" 'kill-this-buffer
   "bi" 'indent-buffer
   "bl" 'ibuffer
   "bm" 'switch-to-messages-buffer
   "bn" 'next-buffer
   "bp" 'previous-buffer
   "br" '(revert-buffer-quick :wk "buffer revert")
   "bR" '(aero/reopen-file-at-buffer :wk "buffer replace")
   "bs" 'switch-to-scratch-buffer
   "bt" '(:ignore t :wk "tabify")
   "btt" 'tabify-buffer
   "btu" 'untabify-buffer
   "bw" '(whitespace-mode :wk "whitespace")
   "bx" 'kill-buffer-and-window

   "c" '(:ignore t :wk "compile")
   "cC" '(aero/byte-recompile-file-at-buffer :wk "byte recompile file at buffer")
   "cR" 'byte-recompile-file
   "cc" 'compile
   "ce" '(:ignore t :wk "elisp")
   "ceB" '(:ignore t :wk "byte compile")
   "ceBF" '(async-byte-compile-file :wk "other file async")
   "ceBb" '(aero/byte-compile-file-at-buffer :wk "file at buffer")
   "ceBd" '(byte-recompile-directory :wk "directory")
   "ceBf" '(byte-compile-file :wk "other file")
   "ceBr" '(aero/byte-recompile-file-at-buffer :wk "file at buffer (recompile)")
   "ceb" 'eval-buffer
   "cec" '(:ignore t :wk "compile")
   "cecb" '(aero/native-compile-file-at-buffer :wk "file at buffer")
   "ced" 'eval-defun
   "cei" '(ielm :wk "ielm repl")
   "cer" 'eval-region
   "ck" 'kill-compilation
   "cr" 'recompile

   "d" '(:ignore t :wk "debug")

   "e" '(:ignore t :wk "errors")
   "en" 'next-error
   "ep" 'previous-error

   "f" '(:ignore t :wk "files")
   "fC" '(:ignore t :wk "convert")
   "fCd" '(aero/unix2dos :wk "unix2dos")
   "fCu" '(aero/dos2unix :wk "dos2unix")
   "fD" '(aero/delete-this-file :wk "delete this file")
   "fE" '(aero/sudo-edit :wk "sudo edit")
   "fR" '(aero/rename-this-file-and-buffer :wk "rename this file")
   "fo" '(:ignore t :wk "open special files")
   "foP" 'aero/open-emacs-problems
   "fof" 'aero/open-local-init
   "fot" '(:ignore t :wk "thornlog")
   "fotd" '(aero/thornlog-todo :wk "thornlog todo")
   "fotl" '(aero/thornlog-today :wk "thornlog log")
   "fott" '(aero/thornlog-dir :wk "thornlog all")
   "fw" '(save-buffer :wk "write buffer")
   "fx" '(aero/xdg-open :wk "xdg-open")

   "g" '(:ignore t :wk "git")
   "gf" '(:ignore t :wk "files")

   "h" '(:ignore t :wk "help/manual")
   "hI" 'info-apropos
   "hM" 'woman
   "hd" '(:ignore t :wk "describe")
   "hi" 'info
   "hm" 'man
   "hw" '(:ignore t :wk "which-key")
   "hwm" '(which-key-show-major-mode :wk "major mode map")

   "j" '(:ignore t :wk "jump")
   "l" '(:ignore t :wk "lsp")

   "m" '(:ignore t :wk "mode")
   "m" '(tmm-menubar :wk "Context menu")

   "o" '(:ignore t :wk "org")
   "oa" 'org-agenda
   "oe" '(:ignore t :wk "org edit")
   "oet" '(:ignore t :wk "org table")
   "oets" 'org-table-sort-lines
   "ot" 'aero/task

   "p" '(:ignore t :wk "project")
   "p/" 'counsel-rg
   "pr" '(xref-find-definitions :wk "find ref")
   "q" '(:ignore t :wk "quoted insert")
   "ql" 'insert-lambda
   "qq" 'quoted-insert

   "s" '(:ignore t :wk "sexp")

   "t" '(:ignore t :wk "tabs/text")
   "td" 'downcase-dwim
   "tu" 'upcase-dwim
   "tf" 'fill-paragraph
   "tF" 'aero/fill-to-80
   "tn" '(:ignore t :wk "number")
   "tnd" 'decrement-number-at-point
   "tni" 'increment-number-at-point
   "ts" 'sort-lines

   "u" 'undo-tree-visualize

   "w" '(:ignore t :wk "window/web")
   "w=" 'balance-windows
   "wB" '(aero/switch-to-minibuffer-window :wk "switch to minibuffer")
   "wF" 'make-frame
   "wL" '(:ignore t :wk "layout")
   "wL2" 'aero/layout-two-columns
   "wL3" 'aero/layout-three-columns
   "ws" '(eww-search-words :which-key "web search")
   "ww" 'eww
   "wb" '(:ignore t :wk "browse")
   "wbb" 'eww-list-buffers
   "wbh" 'eww-list-histories
   "wbm" 'eww-list-bookmarks
   "wbp" 'browse-url-at-point
   "wc" 'aero/toggle-compilation-buffer
   "wd" 'delete-window
   "wh" 'windmove-left
   "wi" 'minimize-window
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right
   "wm" 'maximize-window
   "wo" 'browse-url-xdg-open
   "w{" 'shrink-window
   "w}" 'enlarge-window

   "z" 'repeat))

(use-package which-key :straight t
  :hook (on-first-input . which-key-mode)
  :defines which-key-mode
  :config
  (which-key-mode)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")))


;; we descend to hell

(use-package evil :straight t
  :init
  ;; Need to be in init because of something in the way the "want" variables are used
  (setq evil-want-keybinding nil ; handled by evil-collection
        ;; to change undo-system without restart, use SPC-: `evil-set-undo-system'
        evil-undo-system 'undo-tree
        evil-want-fine-undo t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t)

  :config

  ;; We use SPC as the leader key so it shouldn't do anything when in motion
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

  ;; Ensure horizontal movement doesn't cross to the next/previous line
  (setq-default evil-cross-lines nil)

  ;; By default, these two operate on half pages, but I prefer the smaller jump
  (defun aero/scroll-quarter-page-down ()
    (interactive)
    (evil-scroll-down (/ (window-body-height) 4)))
  (defun aero/scroll-quarter-page ()
    (interactive)
    (evil-scroll-up (/ (window-body-height) 4)))
  (evil-define-key nil global-map (kbd "C-u") #'aero/scroll-quarter-page-up)
  (evil-define-key nil global-map (kbd "C-d") #'aero/scroll-quarter-page-down)

  ;; Very useful, in visual mode, use < and > to indent/unindent the line(s)
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

  ;; :q should kill the current buffer rather than quitting Emacs entirely
  (evil-ex-define-cmd "q" 'kill-this-buffer)

  ;; Unless I'm mistaken, there's no Evil backward equivalent to "e", so we'll invent it.
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-e") #'evil-backward-word-end)
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-M-e") #'evil-backward-WORD-end)

  ;; Useful for pasting into the minibuffer where Evil modes usually don't properly function
  (evil-define-key '(insert) global-map
    (kbd "C-y") #'evil-paste-after)
  (evil-define-key '(insert) global-map
    (kbd "C-S-y") #'evil-paste-before)

  ;; Run macro in register q
  (evil-define-key 'normal 'global "Q" "@q")
  (evil-define-key 'visual 'global
    ;; run macro in register q on region
    "Q" (kbd ":norm @q RET")
    ;; repeat on region
    "." (kbd ":norm . RET"))

  ;; activate
  (evil-mode +1))

;; Provides defaults for many modes which evil proper overlooks
(use-package evil-collection :straight t :after evil :config (evil-collection-init))

;; Doesn't do anything for GUI, so don't bother. In TUI, use a line when in insert mode
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :after evil
    :functions (evil-terminal-cursor-changer-activate)
    :config (evil-terminal-cursor-changer-activate)))

(use-package evil-matchit :straight t :defer 5
  ;; allows % to jump matching tags
  :after evil
  :defines global-evil-matchit-mode
  :config (global-evil-matchit-mode 1))


;; tree-sitter

;; Requires module support
(when (aero/has-modules-p)
  (use-package tree-sitter-langs :straight t :after tree-sitter :defer t)
  (use-package tree-sitter :straight t
	  :after tree-sitter-langs
	  :config (global-tree-sitter-mode +1))

  (use-package evil-textobj-tree-sitter
  	:straight (:host github :repo "meain/evil-textobj-tree-sitter" :files (:defaults "queries"))
  	:after (tree-sitter evil)
    :config
    ;; Annoyingly provides no recommended bindings options, so we have to do it ourselves

    ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
    (define-key evil-outer-text-objects-map "f"
      (evil-textobj-tree-sitter-get-textobj "function.outer"))
    ;; bind `function.inner`(function block without name and args) to `f` for use in things like
    ;; `vif`, `yif`
    (define-key evil-inner-text-objects-map "f"
      (evil-textobj-tree-sitter-get-textobj "function.inner"))

    ;; Sort of a "dwim", matching the first object found, so vaa, etc.
    (define-key evil-outer-text-objects-map "a"
      (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

    ;; Goto start of next function
    (define-key evil-normal-state-map
      (kbd "]f") (lambda ()
                   (interactive)
                   (evil-textobj-tree-sitter-goto-textobj "function.outer")))
    ;; Goto start of previous function
    (define-key evil-normal-state-map
      (kbd "[f") (lambda ()
                   (interactive)
                   (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
    ;; Goto end of next function
    (define-key evil-normal-state-map
      (kbd "]F") (lambda ()
                   (interactive)
                   (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
    ;; Goto end of previous function
    (define-key evil-normal-state-map
      (kbd "[F") (lambda ()
                   (interactive)
                   (evil-textobj-tree-sitter-goto-textobj "function.outer" t t))))

  (use-package turbo-log :straight (:host github :repo "Artawower/turbo-log")
    :after (general tree-sitter)
    :commands (turbo-log-print
               turbo-log-print-immediately
               turbo-log-comment-all-logs
               turbo-log-uncomment-all-logs
               turbo-log-paste-as-logger
               turbo-log-paste-as-logger-immediately
               turbo-log-delete-all-logs)
    :custom
    (turbo-log-msg-format-template "\"🚀: %s\"")
    (turbo-log-allow-insert-without-tree-sitter-p t) ; still works without tree-sitter
    :init
    (aero-leader-def
      "tl" '(:ignore t :wk "turbo-log")
      "tll" 'turbo-log-print
      "tli" 'turbo-log-print-immediately
      "tlh" 'turbo-log-comment-all-logs
      "tls" 'turbo-log-uncomment-all-logs
      "tly" 'turbo-log-paste-as-logger
      "tlY" 'turbo-log-paste-as-logger-immediately
      "tsd" 'turbo-log-delete-all-logs)))

;; abo-abo!

(use-package counsel :straight t
  :after general
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

(use-package amx :straight t
  ;; Enhances counsel-M-x by showing recently used commands and keyboard shortcuts
  :after (counsel)
  :config (amx-mode 1))

(use-package recentf :defer 1
  ;; Doesn't seem like indent activates properly for me without this intervention. Here we move it
  ;; to a known cache file and set up an auto-save every 5 minutes.
  :defines (recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
        recentf-max-saved-items 500)
  (recentf-mode 1)
  (run-at-time 60 (* 5 60) 'recentf-save-list))

(use-package ivy :straight t
  ;; Despite a general trend in the (loud part of the) community to move away from ivy, I'm still a
  ;; big fan. It's fast, its fully-featured and it has many useful integrations.
  :after general
  :config
  ;; Note: flx is a popular fuzzy matching package, but it refuses to prioritize exact matches,
  ;; which gets too annoying to use.

  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil ; don't pre-populate our search
        ivy-use-virtual-buffers t ; add recentf to `ivy-switch-buffer'
        ivy-virtual-abbreviate 'full
        ;; Counting results isn't really that useful and slows down searching large projects
        ;; significantly, so just forget it.
        ivy-count-format ""
        ivy-wrap t ; wrap top to bottom
        ivy-height 12
        ivy-fixed-height-minibuffer t ; better visual consistency
        ivy-on-del-error-function #'ignore ; don't punish me when I accidentally delete search
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (aero-leader-def
    "bb" 'ivy-switch-buffer
    "R" 'ivy-resume))

(use-package ivy-rich :straight t
  ;; Adds information about various results in the ivy buffer
  :after (counsel ivy)
  :defines (ivy-rich-path-style)
  :functions (ivy-rich-mode)
  :config
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))

(use-package all-the-icons :straight t
  :after (general)
  :config (aero-leader-def "qi" 'all-the-icons-insert))

;; Add icons to ivy via ivy-rich
(use-package all-the-icons-ivy-rich
  ;; TEMP https://github.com/seagle0128/all-the-icons-ivy-rich/pull/29
  :straight (:host github :repo "seagle0128/all-the-icons-ivy-rich"
             :fork (:host github :repo "thornjad/all-the-icons-ivy-rich" :branch "use_project-root"))
  :after (all-the-icons ivy-rich)
  :functions (all-the-icons-ivy-rich-mode)
  :config (all-the-icons-ivy-rich-mode +1))

(use-package swiper :straight t
  ;; Search utility
  :after (general counsel)
  :commands (swiper counsel-grep-or-swiper swiper-thing-at-point)
  :init
  (aero-leader-def
    "/" '(counsel-grep-or-swiper :wk "search")
    "?" '(swiper-thing-at-point :wk "search thing at point"))
  :config
  (setq swiper-action-recenter t))

(use-package avy :straight t
  ;; visual navigation utility
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jl" '(avy-goto-line :wk "jump to line")
   "jc" '(avy-goto-char :wk "jump to char")
   "jj" '(avy-goto-char :wk "jump to char")
   "jw" '(avy-goto-word-1 :wk "jump to word")))

(use-package ace-link :straight (:host github :repo "abo-abo/ace-link")
  ;; jump to search results in eww
  :after (avy eww)
  :functions (ace-link-setup-default)
  :config (ace-link-setup-default))

(use-package smartscan :straight t
  ;; Gives us the M-n and M-p symbol following ability
  :after (general)
  :hook (prog-mode . smartscan-mode))


;;; system

(use-package undo-tree :straight t
  :custom
  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)

  :config
  ;; La persistance d'undo-tree est cassée, donc nous écrivons le fichier nous-mêmes. Nous utilisons
  ;; before-save-hook pour que la zone d'écho affiche toujours le message d'écriture du fichier, ce
  ;; qui est plus significatif dans 99% des cas.
  (add-hook 'before-save-hook (lambda () (undo-tree-save-history nil t)))

  (global-undo-tree-mode +1))

(use-package winner
  :after (general)
  :defines winner-boring-buffers
  :config
  ;; list of buffers that winner-undo won't restore
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
  (aero-leader-def
    "wu" 'winner-undo
    "wU" 'winner-redo)

  ;; These don't really always work, and I'm not sure why
  (global-set-key (kbd "M-h") #'windmove-left)
  (global-set-key (kbd "M-j") #'windmove-down)
  (global-set-key (kbd "M-k") #'windmove-up)
  (global-set-key (kbd "M-l") #'windmove-right)
  (when (require 'bind-key nil t)
    (bind-keys*
     ("M-h" . windmove-left)
     ("M-j" . windmove-down)
     ("M-k" . windmove-up)
     ("M-l" . windmove-right))))

(use-package winum :straight t :defer 5
  ;; Jump to windows by number. 1 is the upper-left-most
  :after (general which-key)
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

  ;; collapse all those window commands to one summary in which-key
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
        which-key-replacement-alist)
  (push '((nil . "select-window-[1-9]") . t) which-key-replacement-alist))

;; popup window management
(use-package popper :straight t
  :after general
  :custom
  (popper-window-height 20)
  :config
  (setq popper-reference-buffers ; these are "auto" popups
        '("\\*Messages\\*"
          "\\*vterm.*"
          "\\*LSP Lookup\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*compilation.*"))
  (aero-leader-def
    "wp" '(:ignore t :wk "popper")
    "wpp" 'popper-toggle-latest
    "wpn" 'popper-cycle
    "wpt" 'popper-toggle-type)
  (popper-mode +1))

(use-package helpful :straight t
  ;; Improved version of help buffers
  :commands (helpful-function
             helpful-variable
             helpful-macro
             helpful-key
             helpful-callable)
  :after (evil general)
  :init

  ;; REVIEW See Wilfred/elisp-refs#35. Remove once fixed upstream.
  (unless (version< emacs-version "29")
    (defvar read-symbol-positions-list nil))

  ;; HACK `help-fns--autoloaded-p's signature changed on Emacs 29. This
  ;; suppresses the error until it is addressed upstream. Basically we just
  ;; override the function to ignore the second argument.
  (unless (version< emacs-version "29")
    (advice-add #'help-fns--autoloaded-p :around
                (lambda (fn sym &rest args)
                  (apply fn (list sym)))))

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
  (declare-function aero/pbcopier-select-text "aero-pbcopier.el")
  (declare-function aero/pbcopier-selection-value "aero-pbcopier.el")
	(setq interprogram-cut-function #'aero/pbcopier-select-text)
	(setq interprogram-paste-function #'aero/pbcopier-selection-value)

  (setq-default ns-use-native-fullscreen nil)
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF")))

;; Linux just needs the functionality enabled
(when (system-is-linux)
  (setq select-enable-clipboard t
        interprogram-paste-function #'gui-selection-value
        x-gtk-use-system-tooltips t
        dired-listing-switches "-lFaGh1v --group-directories-first"))

;; TTY also needs some clipboard help. Only works in certain term emulators.
(unless (display-graphic-p)
  (use-package xclip :straight t :config (xclip-mode +1)))


;; File navigation

(use-package tramp :defer t
  :straight (tramp :host nil :repo "git://git.savannah.gnu.org/tramp.git")
  :functions tramp-cleanup-all-connection
  :config
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.config/emacs/data/tramp"
        tramp-use-ssh-controlmaster-options nil  ; use system settings instead
        tramp-default-method "rsync"
        tramp-terminal-type "tramp")

(use-package ranger :straight t
  ;; We only use this for the deer function, which is a better version of dired.
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
  ;; homebrewed pomodoro timer
  :straight (:host gitlab :repo "thornjad/pomp")
  :after (general evil)
  :commands (pomp)
  :init
  (evil-set-initial-state 'pomp-mode 'emacs)
  (global-set-key (kbd "<f12>") 'pomp)
  (aero-leader-def "ap" 'pomp))

(use-package editorconfig :straight t :defer 1
  :functions (editorconfig-mode)
  :config (editorconfig-mode +1))

;; Ensure emacsclient frames open with focus
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

;; startup profiler
(use-package esup :straight t :commands (esup))

(use-package writegood-mode
  ;; Mark passive voice, duplicate words and weasel words
  :straight (:host github :repo "bnbeckwith/writegood-mode")
  :hook ((text-mode) . writegood-mode))

(use-package eprime-mode
  ;; Mark E′ violations
  :straight (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
  :after (general)
  :commands (eprime-check-buffer
             eprime-mode)
  :init
  (aero-leader-def
    "tp" 'eprime-check-buffer
    "tP" 'eprime-mode))

;; REVIEW not sure if we still want this, disabling to see if i miss it
(use-package counsel-spotify :straight t :disabled t
  :after (counsel general)
  :commands (counsel-spotify-toggle-play-pause
             counsel-spotify-next
             counsel-spotify-previous)
  :init
  (aero-leader-def
    "as" '(:ignore t :which-key "spotify")
    "asp" 'counsel-spotify-toggle-play-pause
    "asn" 'counsel-spotify-next
    "asP" 'counsel-spotify-previous))

(use-package unmodified-buffer :defer 1
  ;; detects when the buffer matches what's on disk and marks it unmodified. If, for example, you
  ;; visit a file, change something, then undo the change, this package ensures the buffer doesn't
  ;; think its still modified.
  :straight (:host github :repo "arthurcgusmao/unmodified-buffer")
  :hook ((prog-mode text-mode) . unmodified-buffer-mode))

(use-package virtual-comment :straight t
  ;; Not working well in Emacs 29, and doesn't persist through buffer destruction.
  ;; Use the bindings below to insert a virtual comment which displays in the buffer but never saves
  ;; to disk.
  :hook (find-file-hook . virtual-comment-mode)
  :after (general)
  :init
  (aero-leader-def
    "v" '(:ignore t :wk "virtual comment")
    "vv" 'virtual-comment-make
    "vn" 'virtual-comment-next
    "vp" 'virtual-comment-previous
    "vk" 'virtual-comment-delete
    "vP" 'virtual-comment-paste
    "vs" 'virtual-comment-show))

(use-package clue :defer t
  ;; Method for linking notes to specific locations in files. Rather cumbersome to use. REVIEW may
  ;; want to remove this.
  :straight (:host github :repo "AmaiKinono/clue")
  :after (general)
  :hook (find-file-hook . clue-auto-enable-clue-mode)
  :commands (clue-copy
             clue-paste)
  :custom
  (clue-project-root-function #'projectile-project-root)
  (clue-auto-enable-modes '(markdown-mode))
  :init
  (aero-leader-def
    "Cc" 'clue-copy
    "Cp" 'clue-paste))

;; Keep track of commands used for fun and profit
(use-package keyfreq :straight t
  :defer 1
  :custom (keyfreq-excluded-command '(self-insert-command))
  :config
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))


;; Games, etc.

;; Typing game
(use-package typing
  ;; Note that master is not the default branch in this repo, so we must specify
  :straight (:host github :repo "emacsmirror/emacswiki.org" :files ("typing.el") :branch "master")
  :commands (typing-of-emacs))


(provide 'aero-prelude)

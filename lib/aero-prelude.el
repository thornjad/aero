;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
;;
;; Copyright (c) 2018-2020 Jade Michael Thornton
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
(require 'aero-core)
(require 'aero-lib)

;;; Code:

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(eval-when-compile (declare-function straight-use-package "straight"))
(straight-use-package 'org)


;;; get ready to patch at any time

(use-package el-patch
  :straight (:host github
             :repo "raxod502/el-patch"
             :branch "develop")
  :init
  (setq el-patch-enable-use-package-integration t))
;; Only needed at compile time
(eval-when-compile
  (require 'el-patch))


;;; used in several places

(use-package ripgrep :straight t)


;;; the general is here

(use-package which-key ;; local
  :defines which-key-mode
  :config
  (which-key-mode)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")))

(use-package general :straight t
  :init
  (setq-default general-override-states
                '(insert hybrid normal visual motion operator replace))
  :config
	(general-create-definer aero-leader-def
		:states '(normal visual emacs)
		:prefix "SPC"
		:non-normal-prefix "C-SPC")

  (general-define-key
   :states '(normal visual motion)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "" nil))


;; we descend to hell

(use-package evil
  ;; local
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
   "cm" 'evil-make)

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

  (evil-mode 1))

(use-package evil-matchit :straight t
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

  (general-define-key
   :states '(normal visual insert replace)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" 'counsel-M-x
   "ff" 'counsel-find-file
   "fl" 'counsel-locate
   "fr" 'counsel-recentf
   "?" 'counsel-rg
   "gg" '(counsel-git-grep :which-key "git grep")
   "gff" '(counsel-git :which-key "find git file")
   "ry" '(counsel-yank-pop :which-key "search kill ring")
   "hda" '(counsel-apropos :which-key "apropos")
   "qu" '(aero/counsel-unicode-char-after :which-key "unicode char")
   "qU" 'counsel-unicode-char))

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

(use-package ivy :straight t
  :after general
  :functions ivy-mode
  :config
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
                                (t . ivy--regex-fuzzy)))
  (general-define-key
   :states '(normal)
   :prefix "SPC"

   "bb" 'ivy-switch-buffer)

	(when (display-graphic-p)
		(use-package ivy-posframe :straight t
		:commands (ivy-posframe-mode)
		:defines (ivy-posframe-display-functions-alist
							ivy-posframe-height-alist
							ivy-posframe-parameters
							ivy-posframe-width)
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
          ivy-posframe-height-alist '((t . 20))
          ivy-posframe-parameters '((internal-border-width . 10))
          ivy-posframe-width 140)
    (ivy-posframe-mode 1)))

	(use-package ivy-prescient
		:straight t
		:hook (ivy-mode . ivy-prescient-mode)
		:defines (prescient-filter-method
							prescient-save-file
							ivy-prescient-retain-classic-highlighting)
		:commands (prescient-persist-mode)
		:init
		(setq prescient-filter-method '(fuzzy literal regexp initialism))
		:config
		(setq prescient-save-file (expand-file-name "prescient-save.el" aero-cache-dir))
		(prescient-persist-mode +1))

  (use-package ivy-rich :straight t
    :config
    (ivy-rich-mode 1)))

(use-package swiper :straight t
  :after general
  :commands swiper
  :init
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   "/" '(counsel-grep-or-swiper :which-key "search")))

(use-package flx :straight t)

(use-package avy :straight t
  :commands (avy-goto-line avy-goto-char avy-goto-word-1)
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jl" '(avy-goto-line :which-key "jump to line")
   "jc" '(avy-goto-char :which-key "jump to char")
   "jw" '(avy-goto-word-1 :which-key "jump to word")))


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
   "0" '(winum-select-window-0 :which-key "window-0")
   "1" '(winum-select-window-1 :which-key "window-1")
   "2" '(winum-select-window-2 :which-key "window-2")
   "3" '(winum-select-window-3 :which-key "window-3")
   "4" '(winum-select-window-4 :which-key "window-4")
   "5" '(winum-select-window-5 :which-key "window-5")
   "6" '(winum-select-window-6 :which-key "window-6")
   "7" '(winum-select-window-7 :which-key "window-7")
   "8" '(winum-select-window-8 :which-key "window-8")
   "9" '(winum-select-window-9 :which-key "window-9")
   "wg" '(winum-select-window-by-number :which-key "select window by number"))

  ;; collapse all those window commands to one summary
  (push '(("\\(.*\\) 0" . "winum-select-window-0") . ("\\1 0..9" . "window 0..9"))
        which-key-replacement-alist)
  (push '((nil . "select-window-[1-9]") . t) which-key-replacement-alist))

;; windmove
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-j") #'windmove-down)
(global-set-key (kbd "M-k") #'windmove-up)
(global-set-key (kbd "M-l") #'windmove-right)

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

(use-package pbcopier
  ;; local
  :config
  (turn-on-pbcopier))

(use-package re-builder
  :commands re-builder
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
        tramp-persistency-file-name "~/.config/emacs/data/tramp"

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
  :after general
  ;; local
  :commands pomp
  :init
  (evil-set-initial-state 'pomp-mode 'emacs)
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "ap" 'pomp))


;;; language server protocol interaction

(use-package lsp-mode :straight t
	:defer t
  :after general
  :commands (lsp lsp-deferred)
  :defines (lsp-prefer-flymake
						lsp-session-file
            lsp-enable-snippet
            lsp-resolve-final-function
            lsp-restart
            lsp-language-id-configuration)
	:init
	(setq lsp-session-file (expand-file-name "lsp-session" aero-etc-dir)
				lsp-auto-guess-root t
				lsp-keep-workspace-alive nil
				lsp-prefer-flymake nil)

;;   (defun aero--advice-lsp-mode-silence (format &rest args)
;;     "Silence needless diagnostic messages from `lsp-mode'. This is a
;; `:before-until' advice for several `lsp-mode' logging functions."
;;     (or
;;      ;; Messages we get when trying to start LSP (happens every time we open a
;;      ;; buffer).
;;      (member format `("No LSP server for %s(check *lsp-log*)."
;;                       "Connected to %s."
;;                       ,(concat
;;                         "Unable to calculate the languageId for current "
;;                         "buffer. Take a look at "
;;                         "lsp-language-id-configuration.")))
;;      ;; Errors we get from gopls for no good reason (I can't figure out why).
;;      ;; They don't impair functionality.
;;      (and (stringp (car args))
;;         (or (string-match-p "^no object for ident .+$" (car args))
;;            (string-match-p "^no identifier found$" (car args))))))
;;   (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
;;     (advice-add fun :before-until #'aero--advice-lsp-mode-silence))

  ;; Ignore yasnippet crap, since its not used
  (setq lsp-enable-snippet nil)

  ;; (aero/defadvice
  ;;     aero--lsp-run-from-node-modules (command)
  ;;   :filter-return lsp-resolve-final-function
  ;;   "Find LSP executables inside node_modules/.bin if present."
  ;;   (cl-block nil
  ;;     (prog1 command
  ;;       (when-let ((project-dir
  ;;                   (locate-dominating-file default-directory "node_modules"))
  ;;                  (binary
  ;;                   (aero/path-join
  ;;                    project-dir "node_modules" ".bin" (car command))))
  ;;         (when (file-executable-p binary)
  ;;           (cl-return (cons binary (cdr command))))))))

;;   (aero/defhook
;;       aero--lsp-teardown ()
;;     kill-emacs-hook
;;     "Ignore the LSP server getting killed. If we don't do this, then when killing
;; Emacs we may be prompted with whether we want to restart the LSP server that has
;; just been killed (which happens during Emacs shutdown)."
;;     (setq lsp-restart nil))

  ;; `lsp-mode' doesn't know about LaTeX yet.
  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; fix some bad regexps
  ;; (setq lsp-language-id-configuration
  ;;       (mapcar
  ;;        (lambda (link)
  ;;          (if (and (stringp (car link))
  ;;                 (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
  ;;              (cons
  ;;               (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
  ;;            link))
  ;;        lsp-language-id-configuration))
	)

(use-package lsp-ui :straight t
	:defer t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp :straight t
  :defer t
	:after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package expand-region :straight t
  :defer nil
  :config
  (general-define-key
   :states '(normal visual motion replace emacs)
   :keymaps 'override
    (kbd "C-e") 'er/expand-region))

(use-package beacon :straight t
  :config
  (beacon-mode 1))


;;; general bindings

(general-def
  ;; Emacs chose ^? for the help system for some despicable reason. Fuck that.
  (kbd "C-h") 'delete-backward-char
  (kbd "C-w") 'aero/smarter-backward-kill-word)

(evil-define-key 'normal 'global
  ;; Run macro in register q
  "Q" "@q")
(evil-define-key 'visual 'global
  ;; run macro in register q on region
  "Q" (kbd ":norm @q RET")
  ;; repeat on region
  "." (kbd ":norm . RET"))

(general-define-key
 :states '(normal insert motion)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "" nil

 ;; independent keys
 "TAB" '(aero/alternate-buffer :which-key "alternate buffer")
 (kbd "ESC") 'keyboard-quit
 (kbd "C-g") 'keyboard-quit
 "'" 'eshell
 "\"" '(aero/eshell-new :which-key "eshell-new")
 ":" 'eval-expression
 ";" 'comment-or-uncomment-region
 "!" 'shell-command
 "=" 'quick-calc
 "," '(:ignore t :which-key "mode") ; reserved for mode-specific

 "U" 'universal-argument
 "z" 'repeat

 "q" '(:ignore t :which-key "quoted insert")
 "qq" 'quoted-insert
 "ql" 'insert-lambda

 "f" '(:ignore t :which-key "files")
 "fw" '(save-buffer :which-key "write buffer")
 "fC" '(:ignore t :which-key "convert")
 "fCd" '(aero/unix2dos :which-key "unix2dos")
 "fCu" '(aero/dos2unix :which-key "dos2unix")
 "fD" '(aero/delete-this-file :which-key "delete this file")
 "fE" '(aero/sudo-edit :which-key "sudo edit")
 "fR" '(aero/rename-this-file-and-buffer :which-key "rename this file")
 "fT" 'counsel-load-theme
 "fo" '(:ignore t :which-key "open special files")
 "fod" '(aero/open-tweaks :which-key "tweaks")
 "fot" '(aero/thornlog :which-key "thornlog")
 "foD" '(aero/reload-tweaks :which-key "reload tweaks")

 "h" '(:ignore t :which-key "help/manual")
 "hm" 'woman
 "hM" 'man
 "hi" 'info
 "hI" 'info-apropos
 "hd" '(:ignore t :which-key "describe")

 "b" '(:ignore t :which-key "buffers")
 "bn" 'next-buffer
 "bp" 'previous-buffer
 "bl" 'ibuffer
 "bL" 'list-buffers
 "bm" 'switch-to-messages-buffer
 "br" '(aero/reopen-file-at-buffer :wk "reopen file")
 "bs" 'switch-to-scratch-buffer
 "bd" 'kill-this-buffer
 "bx" 'kill-buffer-and-window
 "bD" '(:ignore t :wk "display")
 "bDw" '(whitespace-mode :wk "whitespace")
 "bt" '(:ignore t :which-key "tabify")
 "btu" 'untabify-buffer
 "btt" 'tabify-buffer
 "bi" 'indent-buffer
 "bP" 'aero/toggle-prettify-this-buffer

 "e" '(:ignore t :which-key "emacs")
 "ea" 'aero/apologize-to-emacs

 "a" '(:ignore t :which-key "applications")
 "ad" 'counsel-dired

 "c" '(:ignore t :which-key "compile")
 "cc" 'compile
 "cC" '(aero/byte-recompile-file-at-buffer :wk "byte recompile file at buffer")
 "ck" 'kill-compilation
 "cr" 'recompile
 "cR" 'byte-recompile-file
 "ce" '(:ignore t :which-key "elisp")
 "cei" '(ielm :which-key "ielm repl")
 "cer" 'eval-region
 "ceb" 'eval-buffer
 "ced" 'eval-defun
 "cec" '(:ignore t :wk "byte compile")
 "cecb" '(aero/byte-compile-file-at-buffer :wk "file at buffer")
 "cecr" '(aero/byte-recompile-file-at-buffer :wk "file at buffer (recompile)")
 "cecf" '(byte-compile-file :wk "other file")
 "cecF" '(async-byte-compile-file :wk "other file async")
 "cecd" '(byte-recompile-directory :wk "directory")

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
 "gf" '(:ignore t :which-key "files")

 "j" '(:ignore t :which-key "jump")
 "s" '(:ignore t :which-key "sexp")
 "m" '(:ignore t :which-key "mode")
 "j" '(:ignore t :which-key "jump")

 "p" '(:ignore t :which-key "project")
 "pr" '(xref-find-definitions :which-key "find ref")

 "u" 'undo-tree-visualize

 "S" '(:ignore t :which-key "shell/sql")
 "Se" 'eshell
 "SE" '(:ignore t :which-key "eshell")
 "St" '(:ignore t :wk "term")

 "w" '(:ignore t :which-key "window/web")
 "w=" 'balance-windows
 "w2" 'aero/layout-two-columns
 "w3" 'aero/layout-three-columns
 "wB" '(aero/switch-to-minibuffer-window :which-key "switch to minibuffer")
 "wd" 'delete-window
 "wF" 'make-frame
 "wx" 'kill-buffer-and-window
 "w{" 'shrink-window
 "w}" 'enlarge-window

 "t" '(:ignore t :which-key "text")
 "tU" 'upcase-dwim
 "tD" 'downcase-dwim
 "tn" '(:ignore :wk "number")
 "tni" 'increment-number-at-point
 "tnd" 'decrement-number-at-point
 )

(provide 'aero-prelude)

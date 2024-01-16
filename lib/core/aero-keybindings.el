;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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
;;; Code:

;; General lets us more easily set keybindings throughout Aero
(package! general :auto
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
    (kbd "M-TAB") 'aero/alternate-buffer
    (kbd "C-RET") 'aero/browse-url-open)

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
   ";" 'aero/comment-dwim
   "!" 'shell-command
   "=" 'quick-calc

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

   "P" '(:ignore t :wk "packages")
   "PC" 'straight-check-all
   "PF" 'straight-fetch-all
   "PP" 'straight-pull-all
   "PR" 'straight-rebuild-all
   "Pg" 'straight-get-recipe
   "Pp" 'straight-pull-package-and-deps
   "Pr" 'straight-rebuild-package
   "Px" 'straight-prune-build

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
   "ai" '(:ignore t :wk "AI functions")
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
   "br" '(aero/reopen-file-at-buffer :wk "buffer replace")
   "bR" '(revert-buffer-quick :wk "buffer revert")
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

   "f" '(:ignore t :wk "files")
   "fc" 'aero/copy-file-relative-to-project
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
   "fotn" '(aero/thornlog-notes :wk "thornlog notes")
   "fott" '(aero/thornlog-dir :wk "thornlog all")
   "fw" '(save-buffer :wk "write buffer")
   "fx" '(aero/xdg-open :wk "xdg-open")
   "fh" '(aero/toggle-angular-component-file :wk "toggle angular component file")

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

   "r" '(:ignore t :wk "xref")
   "rf" 'xref-find-definitions
   "rF" 'xref-find-definitions-other-window
   "rp" 'xref-go-back
   "rn" 'xref-go-forward
   "ra" 'xref-find-apropos
   "rr" 'xref-find-references

   "q" '(:ignore t :wk "quoted insert")
   "ql" 'insert-lambda
   "qq" 'quoted-insert
   "qp" 'aero/insert-pdb

   "s" '(:ignore t :wk "sexp")

   "t" '(:ignore t :wk "tabs/text")
   "td" 'dictionary-lookup-definition
   "tD" 'downcase-dwim
   "tU" 'upcase-dwim
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
   "wp" 'browse-url-at-point
   "wc" 'aero/toggle-compilation-buffer
   "wd" 'delete-window
   "wh" 'windmove-left
   "wi" 'minimize-window
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right
   "wm" 'maximize-window
   "wo" 'aero/browse-url-open
   "w{" 'shrink-window
   "w}" 'enlarge-window

   "z" 'repeat))

(package! which-key :auto
  :hook (on-first-input . which-key-mode)
  :defines which-key-mode
  :config
  (which-key-mode)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")))

(provide 'aero-keybindings)
;;; aero-keybindings.el ends here

;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
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
;; After init.el, this file is the core driver of Aero. It sets up ubiquitous packages and the
;; primary keybindings.
;;
;; The file name of "prelude" has nothing to do with the excellent Emacs configuration distribution
;; of the same name. Instead, the name alludes to the fact that the configuration here comes before
;; the rest, and in many cases is required by other packages.

(require 'cl-lib)
(require 'aero-lib)

;;; Code:


;; Set up core packages. The ELPA keyring sometimes gets screwed up, but this package lets us fix
;; it easily.
(package! gnu-elpa-keyring-update :auto)

;; Requirements for lib
(package! memo (:host gitlab :repo "thornjad/emacs-memo" :branch "main"))
(package! async (:host github :repo "jwiegley/emacs-async") :commands (async-save))
(package! popup :auto)
(package! spinner :auto)

;; Mostly only required for MacOS, we need to grab environment variables from the default shell.
;; This lets us use TRAMP more easily and connects us with some tools.
(package! exec-path-from-shell :auto :defer 1
  :config
  (when (or (window-system) (daemonp))
    (dolist (var '("PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "PATH" "MANPATH" "INFOPATH" "LSP_USE_PLISTS" "HOMEBREW_PREFIX" "HOMEBREW_CELLAR" "HOMEBREW_REPOSITORY"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;; Faster than grep, but requires ripgrep to be installed locally
(package! ripgrep :auto :defer 3)

(require 'aero-keybindings)
(require 'aero-evil)
(when (treesitterp) (require 'aero-treesitter))
(require 'aero-completion)

(package! smartscan :auto
  ;; Gives us the M-n and M-p symbol following ability
  :hook (prog-mode . smartscan-mode)
  :config
  (advice-add 'smartscan-symbol-go-forward :around #'aero/advice-disable-subword)
  (advice-add 'smartscan-symbol-go-backward :around #'aero/advice-disable-subword))

(package! undo-tree :auto
  :custom
  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(expand-file-name "undo-tree/" aero-cache-dir))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)

  :config
  (add-hook 'before-save-hook (lambda () (undo-tree-save-history nil t)))

  (defun aero/kill-undo-tree-save-file-for-buffer ()
    "Deletes the `undo-tree' history file for this buffer.
Useful for when undo-tree inevitably fucks up the file and it can't be read."
    (interactive)
    (if buffer-file-name
        (let ((filename (undo-tree-make-history-save-file-name buffer-file-name)))
          (when (y-or-n-p (concat "Kill undo-tree history file " filename "?"))
            (delete-file filename)))
      (message "Buffer is not a file")))

  (advice-add 'undo-tree-save-history :around #'aero/advice-no-message)
  (global-undo-tree-mode +1))

(package! winner :builtin
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

(package! winum :auto
  :defer 5
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

(package! helpful :auto
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
   "hdF" 'counsel-faces
   "hda" 'helpful-symbol
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
  (package! xclip :auto :config (xclip-mode +1)))


;; File navigation

(package! tramp (tramp :host nil :repo "git://git.savannah.gnu.org/tramp.git")
  :defer t
  :functions tramp-cleanup-all-connection
  :config
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.config/emacs/data/tramp"
        tramp-use-ssh-controlmaster-options nil  ; use system settings instead
        tramp-default-method "rsync"
        tramp-terminal-type "tramp"))

(package! ranger :auto
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


;; AI

;; Aero LLM assistant interface
(package! aero-assistant :local :load-path "lib/localpackages/aero-assistant"
	:after markdown-mode
  :commands (aero/assistant aero/assistant-commit-message)
  :custom (aero/assistant-openai-api-key openai-api-key))

;; Required by chatgpt-shell
(package! shell-maker
  (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

;; GPT and DALL-E interface
(package! chatgpt-shell
  (:host github :repo "xenodium/chatgpt-shell")
  :requires shell-maker
  :after general
  :commands (chatgpt-shell
             dall-e-shell chatgpt-shell-send-and-review-region
             chatgpt-shell-write-git-commit chatgpt-shell-explain-code
             chatgpt-shell-proofread-region chatgpt-shell-refactor-code
             chatgpt-shell-restore-session-from-transcript
             chatgpt-shell-generate-unit-test)

  :custom
  (chatgpt-shell-openai-key openai-api-key)
  (dall-e-shell-openai-key openai-api-key)
  (chatgpt-shell-model-versions '("gpt-4-1106-preview" "gpt-3.5-turbo-16k-0613"
                                  "gpt-3.5-turbo"))
  (chatgpt-shell-welcome-function nil) ; disable welcome message
  (chatgpt-shell-system-prompts `(("Aero" . ,aero/assistant-openai-system-prompt)))
  (chatgpt-shell-history-path aero-cache-dir)

  :init
  (aero-leader-def
    "aic" '(chatgpt-shell :wk "chat shell")
    "air" '(chatgpt-shell-send-and-review-region :wk "send region with review")
    "aig" '(chatgpt-shell-write-git-commit :wk "write git commit")
    "aie" '(chatgpt-shell-explain-code :wk "explain code in region")
    "aip" '(chatgpt-shell-proofread-region :wk "proofread in region")
    "aif" '(chatgpt-shell-refactor-code :wk "refactor code in region")
    "ais" '(chatgpt-shell-restore-session-from-transcript :wk "restore session from transcript")
    "aiu" '(chatgpt-shell-generate-unit-test :wk "generate unit test")))

;; Works best with company-box, so we consider it a requirement
(package! copilot (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :after (company-box general)
  :hook ((prog-mode eshell-mode) . copilot-mode)
  :custom (copilot-idle-delay 0.5)
  :config
  (general-define-key
   :states '(insert visual motion)
   :keymaps 'copilot-mode-map
   (kbd "C-<tab>") 'copilot-accept-completion
   (kbd "C-c C-n") 'copilot-next-completion
   (kbd "C-c C-p") 'copilot-previous-completion))


;; Better writing

;; Mark passive voice, duplicate words and weasel words
(package! writegood-mode (:host github :repo "bnbeckwith/writegood-mode")
  :hook ((text-mode) . writegood-mode))

;; Mark E′ violations
(package! eprime-mode (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
  :after (general)
  ;; :hook text-mode
  :commands (eprime-check-buffer eprime-mode)
  :init
  (aero-leader-def
    "tp" 'eprime-check-buffer
    "tP" 'eprime-mode))


;; General crap

;; My pomodoro package
(package! pomp (:host gitlab :repo "thornjad/pomp")
  :after (general evil)
  :commands (pomp)
  :custom
  (pomp-pomodoro-length 55)
  (pomp-short-break-length 10)
  (pomp-long-break-length 15)
  :init
  (aero-leader-def "ap" 'pomp))

(package! editorconfig :auto
  :defer 1
  :functions (editorconfig-mode)
  :config (editorconfig-mode +1))

;; Ensure emacsclient frames open with focus
(add-hook 'server-switch-hook (lambda () (select-frame-set-input-focus (selected-frame))))

;; startup profiler
(package! esup :auto :commands (esup))

;; detects when the buffer matches what's on disk and marks it unmodified. If, for example, you
;; visit a file, change something, then undo the change, this package ensures the buffer doesn't
;; think its still modified.
(package! unmodified-buffer (:host github :repo "arthurcgusmao/unmodified-buffer")
  :defer 1
  :hook ((prog-mode text-mode) . unmodified-buffer-mode))

(package! virtual-comment :auto
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
    "vs" 'virtual-comment-show)
  :config
  (evil-set-initial-state 'virtual-comment-mode 'insert))

;; Use `so-long-revert' in a buffer to get back to what it would otherwise have loaded as.
(package! so-long :builtin
  :config (global-so-long-mode +1))


;; Games, etc.

;; Typing game
(package! typing (:host github :repo "thornjad/emacswiki-typing")
  :commands (typing-of-emacs))

(require 'wttrin (expand-file-name "lib/localpackages/wttrin.el" user-emacs-directory))


(provide 'aero-prelude)
;;; aero-prelude.el ends here

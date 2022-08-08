;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package magit
  :after (general ivy)
	:commands (magit-blame
             ivy-magit-todos
             magit-log-buffer-file
             magit-log-trace-definition
             magit-log-head
             magit-commit
             magit-commit-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-fetch-popup
             magit-init
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-revert
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
	:init
	(aero-leader-def
	  "gs" 'magit-status
	  "gb" 'magit-blame
    "gt" 'ivy-magit-todos
    "gl" '(:ignore t :which-key "log")
    "glb" 'magit-log-buffer-file
    "gld" 'magit-log-trace-definition
    "gll" 'magit-log-head
	  "gfS" 'magit-stage-file
	  "gfU" 'magit-unstage-file
    "gm" '(:ignore t :which-key "smerge")
    "gmm" 'smerge-start-session
    "gmu" 'smerge-keep-upper
    "gml" 'smerge-keep-lower
    "gmn" 'smerge-next
    "gmp" 'smerge-prev
    "gma" 'smerge-keep-all
    "gmE" 'smerge-ediff
    "gmC" 'smerge-combine-with-next
    "gmr" 'smerge-refine
    "gmR" 'smerge-resolve
    "gmd" '(:ignore t :wk "diff")
    "gmdu" '(:ignore t :wk "upper")
    "gmdul" 'smerge-diff-upper-lower
    "gmdb" '(:ignore t :wk "base")
    "gmdbu" 'smerge-diff-base-upper
    "gmdbl" 'smerge-diff-base-lower)

  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (setq magit-completing-read-function 'ivy-completing-read
        magit-buffer-name-format "%x%M%v: %t%x"
        magit-diff-paint-whitespace-lines 'both
        magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t
        git-commit-style-convention-checks '(non-empty-second-line
                                             overlong-summary-line)
        git-commit-summary-max-length 50
        git-commit-fill-column 72)
  (magit-auto-revert-mode nil)
  (defadvice magit-diff (after switch-to-diff activate)
    (other-window 1))
  (add-hook 'magit-status-mode-hook (lambda () (toggle-truncate-lines -1)))

  (defun aero/magit-diff-default-branch (&optional args)
    "Show diff of default branch to working tree."
    (interactive (list (magit-diff-arguments)))
    (magit-diff-working-tree
     (replace-regexp-in-string "refs/remotes/origin/" ""
                               (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))
     args))

  (magit-define-popup-action
   'magit-diff-popup
   ?m "Diff main branch" #'aero/magit-diff-default-branch)

  ;; Don't want no color from the pre-commit hook
  (defun aero/magit--color-buffer (proc &rest args)
    (interactive)
    (with-current-buffer (process-buffer proc)
      (read-only-mode -1)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode 1)))
  (advice-add 'magit-process-filter :after #'aero/magit--color-buffer)

  (defun aero/fetch-pr ()
    "Fetch a GH(E) pull request into a new branch prefixed with `pr'."
    (interactive)
    (let* ((pr (message-read-from-minibuffer "Enter PR number: "))
           (new-branch (format "pr%s" pr))
           (fetch-command
            (format "git fetch origin pull/%s/head:%s" pr new-branch)))
      (shell-command fetch-command)
      (magit-status)
      (message "Checked out PR as %s" new-branch))))

(use-package forge :straight t :after magit
  :custom (forge-database-file (expand-file-name "forge-database.sqlite" aero-cache-dir)))

(use-package magit-todos :straight t
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "TEMP"))))

;; use delta pager in magit diffs
(use-package magit-delta :straight t :hook (magit-mode . magit-delta-mode))

;; extremely difficult to style for some reason
(use-package blamer :straight t
  :commands (blamer-mode)
  :custom
  (blamer-view 'overlay-right)
  (blamer-type 'visual)
  (blamer-max-commit-message-length 180)
  (blamer-author-formatter " ● [%s] ")
  (blamer-commit-formatter "| %s")
  (blamer-idle-time 0.1)
  (blamer-min-offset 15))

(use-package git-gutter :straight t
  :hook ((prog-mode text-mode conf-mode) . git-gutter-mode)
  :custom
  (git-gutter:visual-line t)
  (git-gutter:disabled-modes '(so-long-mode
                               image-mode asm-mode
                               fundamental-mode image-mode pdf-view-mode))
  (git-gutter:update-interval 0.02)
  (git-gutter:handled-backends
   (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                :key #'symbol-name)))
  ;; (git-gutter:modified-sign " ")
  ;; (git-gutter:added-sign " ")
  ;; (git-gutter:deleted-sign " ")

  :config
  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows))

(when (display-graphic-p)
  ;; Define as a no-op if not already defined, otherwise git-gutter-fringe errors
  (unless (fboundp 'define-fringe-bitmap)
    (defun define-fringe-bitmap (bitmap &rest _)
      "This is a no-op placeholder function."
      ;; Return the symbol, just like the normal function does.
      bitmap))

  (use-package git-gutter-fringe :straight t :after (git-gutter)
    :custom
    (fringes-outside-margins t)

    :config
    ;; Define a thin bar. Themes should give these a suitable foreground and nil background
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
      nil nil 'bottom)

    ;; Don't use git-gutter in TRAMP, it murders connection bandwidth
    (defun git-gutter-find-file-hook ()
      (git-gutter-mode
       (if (file-remote-p (buffer-file-name))
           0
         1)))
    (add-hook 'find-file-hook #'git-gutter-find-file-hook)))

(use-package ediff
  :commands (ediff ediff3)
  :custom
  (ediff-split-window-function #'split-window-horizontally )
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package git-link :straight t
  :after (general)
  :commands (git-link git-link-commit git-link-homepage)
  :init (aero-leader-def "gL" 'git-link))

(provide 'aero-git)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2024 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(package! magit :auto
  :after (general)
  :commands (magit-blame
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

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-process-finish-apply-ansi-colors t)
  (magit-buffer-name-format "%x%M%v: %t%x")
  (magit-diff-paint-whitespace-lines 'both)
  (magit-diff-refine-hunk 'all)
  (magit-diff-refine-ignore-whitespace t)
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line))
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)

  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (magit-auto-revert-mode nil)

  (defun aero/truncate-lines-off () (toggle-truncate-lines -1))
  (add-hook 'magit-status-mode-hook #'aero/truncate-lines-off)

  (defun aero/magit-switch-to-diff () (other-window 1))
  (advice-add 'magit-diff :after #'aero/magit-switch-to-diff)

  (defun aero/magit-diff-default-branch (&optional args)
    "Show diff of default branch to working tree."
    (interactive (list (magit-diff-arguments)))
    (magit-diff-working-tree
     (replace-regexp-in-string "refs/remotes/origin/" ""
                               (magit-git-string "symbolic-ref" "refs/remotes/origin/HEAD"))
     args))

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

(package! magit-todos :auto
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config
  (setq magit-todos-recursive t
        magit-todos-depth 10
        magit-todos-exclude-globs '("*Pods*" ".git/" "*elpa*" "*var/lsp/*"))
  (custom-set-variables
   '(magit-todos-keywords (list "TODO" "FIXME" "TEMP"))))

(package! git-gutter :auto
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

  (package! git-gutter-fringe :auto :after (git-gutter)
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

(package! ediff :builtin
  :commands (ediff ediff3)
  :custom
  (ediff-split-window-function #'split-window-horizontally )
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(package! git-link :auto
  :after (general)
  :commands (git-link git-link-commit git-link-homepage)
  :init (aero-leader-def "gL" 'git-link))

(provide 'aero-git)

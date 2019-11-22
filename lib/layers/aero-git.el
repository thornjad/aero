;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package magit :ensure t
	:commands (magit-blame
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
	(general-define-key
	 :states '(normal)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"
	 "gs" 'magit-status
	 "gb" 'magit-blame
	 "gfS" 'magit-stage-file
	 "gfU" 'magit-unstage-file
   "gm" '(:ignore t :which-key "Smerge")
   "gmc" 'smerge-keep-current
   "gmn" 'smerge-next
   "gmp" 'smerge-prev
   "gmo" 'smerge-keep-other
   "gma" 'smerge-keep-all
   "gmE" 'smerge-ediff
   "gmC" 'smerge-combine-with-next
   "gmr" 'smerge-refine)

	:config
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (setq magit-completing-read-function 'ivy-completing-read
        magit-buffer-name-format "%x%M%v: %t%x"
        magit-diff-paint-whitespace-lines 'both
        magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t)
	(global-git-commit-mode t)
  (magit-auto-revert-mode nil)
  (defadvice magit-diff (after switch-to-diff activate)
    (other-window 1))

  (defun aero/fetch-pr ()
    "Fetch a GH(E) pull request into a new branch prefixed with `pr'."
    (interactive)
    (let* ((pr (message-read-from-minibuffer "Enter PR number: "))
           (new-branch (format "pr%s" pr))
           (fetch-command
            (format "git fetch origin pull/%s/head:%s" pr new-branch)))
      (shell-command fetch-command)
      (magit-status)
      (message "Checked out PR as %s" new-branch)))

  (defun aero/insert-jira-ticket ()
    "Add JIRA ticket number to current buffer.

Intended to be used with git commit messages to enable automation in JIRA and
Zeitgit. Requires Magit. Customize MY-BOARDS to all boards you want to match on.

This function will only work if branches are named with the schema
board_ticket_branch_name."
    (interactive)
    (let* ((branch (magit-get-current-branch))
           (parts (split-string branch "_"))
           (board (upcase (car parts)))
           (ticket (cadr parts))
           (my-boards '("WEB")))
      (when (member board my-boards)
        (save-excursion
          (next-line)
          (insert (format "\n%s-%s" board ticket))))))

  ;; Auto-add ticket number when opening commit message
  (add-hook 'git-commit-setup-hook #'aero/insert-jira-ticket)

  (general-define-key
   :states 'normal
   :prefix "SPC"
    "gp" 'aero/fetch-pr
    "qw" 'aero/insert-jira-ticket)

	(use-package magit-todos :ensure t)
	(use-package evil-magit :ensure t))

(use-package ediff
  :commands (ediff ediff3)
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))


(provide 'aero-git)

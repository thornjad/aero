;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2021 Jade Michael Thornton
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
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package projectile
  :straight (:host github :repo "bbatsov/projectile")
	:config
	(setq projectile-indexing-method 'native
				projectile-enable-caching t
				projectile-git-submodule-command nil
				projectile-mode-line nil)
  (projectile-mode 1))

(use-package counsel-projectile 
	:after (projectile general)
	:config

	(general-define-key
	 :states 'normal
	 :prefix "SPC"
   "p/" '(counsel-projectile-rg :wk "find with rg")
   "pf" '(counsel-projectile-find-file-dwim :wk "find file dwim")
   "pp" '(counsel-projectile-switch-project :wk "switch project")
   "p:" '(projectile-run-shell-command-in-root :wk "shell command in root")
   "p&" '(projectile-run-async-shell-command-in-root :wk "async shell command in root")
   "p'" '(projectile-run-shell :wk "run shell in root")
   "p%" '(projectile-replace-regexp :wk "regex replace")
   "pC" '(projectile-compile-project :wk "compile")
   "pt" '(projectile-find-tag :wk "find tag")
   "pG" 'projectile-regenerate-tags
   "pI" 'projectile-invalidate-cache
   "pC" 'projectile-cache-current-file))

(use-package find-file-in-project
  :after (ivy general)
  :straight (:host github :repo "redguardtoo/find-file-in-project")
  :config
  (aero-leader-def
    "pF" 'find-file-in-project))

(use-package citre :straight (:host github :repo "universal-ctags/citre")
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :after (general ace)
  :config
  (aero-leader-def
    "jj" 'citre-jump
    "jb" 'citre-jump-back
    "jp" 'citre-ace-peek
    "jC" 'aero/ctags-create-tags)

  ;; citre doesn't support tramp out of the box, tell it how to get the remote file
  (defadvice citre-core--get-lines (around aero/citre-tramptags-reform activate)
    (if (file-remote-p tagsfile)
        (let ((default-directory (file-name-directory tagsfile))
              (tagsfile (file-local-name tagsfile)))
          ad-do-it)
      ad-do-it))

  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))
  (with-eval-after-load 'cc-mode (require 'citre-lang-c)))

(provide 'aero-project)

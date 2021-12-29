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
	(setq projectile-indexing-method 'alien
				projectile-enable-caching t
				projectile-mode-line nil)
  (projectile-mode 1))

(use-package counsel-projectile 
	:after (projectile general)
	:config
	(aero-leader-def
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

(use-package treemacs :straight t
  :hook ((lsp . treemacs))
  :config
  (use-package treemacs-evil :straight t)
  (use-package treemacs-projectile :straight t)
  (use-package treemacs-magit :straight t)
  (use-package treemacs-all-the-icons :straight t)

  (setq treemacs-tag-follow-delay 0.5)

  ;; Follow me around
  (treemacs-project-follow-mode +1)
  (treemacs-git-mode 'deferred)
  (treemacs-filewatch-mode +1))

(provide 'aero-project)

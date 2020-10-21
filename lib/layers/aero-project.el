;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
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

;; Use `rg' instead of the default `find'
(el-patch-feature project)
(el-patch-defun project--files-in-directory (dir ignores &optional files)
  (el-patch-remove
    (require 'find-dired)
    (require 'xref)
    (defvar find-name-arg))
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (el-patch-swap
                    (format "%s %s %s -type f %s -print0"
                            find-program
                            localdir
                            (xref--find-ignores-arguments ignores localdir)
                            (if files
                                (concat (shell-quote-argument "(")
                                        " " find-name-arg " "
                                        (mapconcat
                                         #'shell-quote-argument
                                         (split-string files)
                                         (concat " -o " find-name-arg " "))
                                        " "
                                        (shell-quote-argument ")"))""))
                    (format "rg --files | rg %s" localdir))))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))


(use-package projectile :straight t
	:config
	(setq projectile-indexing-method 'alien
				projectile-enable-caching t
				projectile-git-submodule-command nil
				projectile-mode-line nil)
  (projectile-mode 1))

(use-package counsel-projectile :straight t
	:after projectile
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

(defun aero/ack-project ()
  "Search for a string in the current project."
  (interactive)

  (require 'projectile)
  (require 'counsel)

  (let ((counsel-ag-base-command "ack --nopager --nocolor --nogroup %s")
        (counsel--grep-tool-look-around t)
        (root-dir (projectile-project-root)))
    (counsel-ag "" root-dir)))

(provide 'aero-project)

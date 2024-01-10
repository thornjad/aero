;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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

(package! project :builtin
  :after (general)
  :custom
  (project-vc-ignores '("node_modules/" "straight/" "target/")) ; globally ignored
  (project-vc-extra-root-markers '(".project.el" ".projectile" ".git"))
  (project-compilation-buffer-name-function #'project-prefixed-buffer-name)

  :config
  (defun aero/project-root-override (dir)
    "Find DIR's project root by searching for a '.project.el' file.

If this file exists, it marks the project root. For convenient compatibility with Projectile, '.projectile' is also considered a project root marker.

https://blog.jmthornton.net/p/emacs-project-override"
    (let ((root (or (locate-dominating-file dir ".project.el")
                    (locate-dominating-file dir ".projectile")))
          (backend (ignore-errors (vc-responsible-backend dir))))
      (when root (if (version<= emacs-version "28")
                     (cons 'vc root)
                   (list 'vc backend root)))))

  ;; Note that we cannot use :hook here because `project-find-functions' doesn't end in "-hook", and
  ;; we can't use this in :init because it won't be defined yet.
  (add-hook 'project-find-functions #'aero/project-root-override)

  ;; Set our own list of actions on `project-switch-project'
  (setq project-switch-commands '((project-find-file "Find file" "f")
                                  (magit-status "Magit status" "g")
                                  (project-eshell "Eshell" "e")
                                  (counsel-rg "Ripgrep" "r")))

  (aero-leader-def
    "pf" 'project-find-file
    "pp" 'project-switch-project
    "p:" 'project-shell-command
    "p&" 'project-async-shell-command
    "p'" 'project-eshell
    "p\"" 'aero/project-eshell-new
    "p%" 'project-query-replace-regexp
    "cp" 'project-compile))

(provide 'aero-project)

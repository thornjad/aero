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

(use-package projectile :straight t
	:config
	(projectile-mode 1)
	(setq projectile-indexing-method 'alien
				projectile-enable-caching t
				;; fix sub-projects bug https://github.com/bbatsov/projectile/issue/1302
				projectile-git-submodule-command nil))

(use-package counsel-projectile :straight t
	:after projectile
	:config
	(general-define-key
	 :states '(normal insert)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"
	 "pf" 'counsel-projectile-find-file))

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

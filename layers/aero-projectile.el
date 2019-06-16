;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package projectile :ensure t
	:config
	(projectile-mode 1)
	(setq projectile-indexing-method 'alien
				projectile-enable-caching t
				;; fix sub-projects bug https://github.com/bbatsov/projectile/issue/1302
				projectile-git-submodule-command nil)
	(general-define-key
	 :states '(normal insert emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"
	 "p" '(:ignore t :which-key "projectile")
	 "pf" 'counsel-projectile-find-file))

(provide 'aero-projectile)

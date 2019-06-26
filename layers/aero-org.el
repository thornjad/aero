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

(use-package org
	:defer t
	:ensure org-plus-contrib
	:commands org-mode
	:mode ("\\.org\\'" . org-mode)

	:config
	(setq org-src-preserve-indentation t
				org-footnote-auto-adjust t
				org-footnote-section nil
				org-startup-with-inline-images t
				org-startup-indented t
				org-hide-leading-stars t)

	;; rescale images to 400px if no with attribute is set (see
	;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
	(setq org-image-actual-width '(400)))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("➡" "➠" "➟" "➝" "↪")))

(use-package org-indent
  :ensure org-plus-contrib
  :after org)

(provide 'aero-org)

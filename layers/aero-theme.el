;; -*- lexical-binding: t -*-
;; Aero Theme
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

(defun aero/load-theme ()
	"Load Aero default theme"
	(use-package doom-themes :ensure t
		:config (load-theme 'doom-opera t))

	(use-package doom-modeline :ensure t
		:config
		(doom-modeline-init)
		(setq doom-modeline-height 20)))

(provide 'aero-theme)

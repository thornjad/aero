;; -*- lexical-binding: t -*-
;; Aero Theme
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
;;
;; License: GPLv3

(defun aero/load-theme ()
	"Load Aero default theme"
	(use-package doom-themes :ensure t
		:config (load-theme 'doom-opera t))

	(use-package doom-modeline :ensure t
		:config
		(doom-modeline-init)
		(setq doom-modeline-height 20)))

(provide 'core-theme)

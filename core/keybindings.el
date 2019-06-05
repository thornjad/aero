;; -*- lexical-binding: t -*-
;; Keybindings
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
;;
;; License: GPLv3

(use-package general
	:ensure t
	:config
	(general-define-key
	 :states '(normal visual insert emacs)
	 :prefix "SPC"
	 :non-normal-prefix "C-SPC"

	 ;; simple commands
	 "TAB" '(switch-to-other-buffer :which-key "prev buffer")))

;; -*- lexical-binding: t -*-
;;
;; Aero core layer utilities
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

;; TODO can these just be loaded by file-name?
;;   - but then how would layers be disabled (it happens)?
(defvar aero-layer-list
	'(
		aero-eww
    aero-feeds
		aero-git
		aero-heap
		aero-lisp
		aero-markup
		aero-prog
		aero-projectile
    aero-python
		aero-restart
    aero-rust
		aero-shell
		aero-sql
    aero-theme
		aero-web
    )
	"A list of local aero layers to install at startup. To include a single,
	non-aero layer, add to aero-heap.el.")

(defun aero/load-layers ()
	"Load all configured layers, listed above"

	;; load up the prelude first, it defines some functions we want in other
	;; layers. You could say it... /preludes/ the other layers
	(require 'aero-prelude)

	;; now the rest
	;; TODO parallel?
	(dolist (layer aero-layer-list)
		(require layer))

	;; and finally, tweaks pseudo-layer
	(require 'aero-tweaks))

(provide 'aero-layers)

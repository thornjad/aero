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
;; TODO alphabetize
(defvar aero-layer-list
	'(aero-theme
		aero-prog
		aero-projectile
		aero-restart
		aero-git
    aero-python
		aero-lisp
		aero-web
		aero-markup
		aero-sql
		aero-shell
		aero-heap
    aero-rust
		aero-eww)
	"A list of layers to ensure at startup

If a layer listed here does not have a corresponding file in the `layers'
directory, it will be loaded without configuration. However, this is not
recommended. Using a configuration file,k even with only a single `use-package'
will be more efficient if compiled.")

(defun aero/require-layer (layer)
	"Install `layer' unless it is already installed"
	(unless (memq layer aero-layer-list)
		(add-to-list 'aero-layer-list layer))
	(unless (package-installed-p layer)
		(require layer)))

;; TODO parallel?
(defun aero/load-layers ()
	"Load all configured layers, listed above"

	;; load up the prelude first, it defines some functions we want in other
	;; layers. You could say it... /preludes/ the other layers
	(aero/require-layer 'aero-prelude)

	;; now the rest
	(dolist (layer aero-layer-list)
		(aero/require-layer layer))

	;; and finally, tweaks pseudo-layer
	(aero/require-layer 'aero-tweaks))

(provide 'aero-layers)

;; -*- lexical-binding: t -*-
;;
;; Aero core layer utilities
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; permission to use, copy, modify, and/or
;; distribute this software for any purpose with or without fee is hereby
;; granted, provided that the above copyright notice and this permission notice
;; appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, Negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.
;;
;; This file is not part of GNU Emacs

(defvar aero-layer-list
	'(aero-prelude
		aero-theme)
	"A list of layers to ensure at startup

If a layer listed here does not have a corresponding file in the `layers'
directory, it will be loaded without configuration.")

(defun aero/require-layer (layer)
	"Install `layer' unless it is already installed"
	(unless (memq layer aero-layer-list)
		(add-to-list 'aero-layer-list layer))
	(unless (package-installed-p layer)
		(require layer)))

(defun aero/load-layers ()
	"Load all configured layers, listed above"
	(dolist (layer aero-layer-list)
		(aero/require-layer layer)))

(provide 'aero-layers)

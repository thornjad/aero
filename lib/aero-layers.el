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

(defun aero/load-layers ()
	"Load all configured layers, listed above"

  (let ((aero-layer-list '(aero-c
                           aero-eww
                           aero-feed
                           aero-git
                           aero-heap
                           aero-lisp
                           aero-markup
                           aero-ml
                           aero-org
                           aero-prog
                           aero-project
                           aero-python
                           aero-rust
                           aero-shell
                           aero-spotify
                           aero-sql
                           aero-tcl
                           aero-theme
                           aero-web)))

    ;; load up the prelude first, it defines some functions we want in other
    ;; layers. You could say it... /preludes/ the other layers
    (require 'aero-prelude)

    ;; now the rest
    ;; TODO parallel?
    (dolist (layer aero-layer-list)
      (require layer))

    ;; load private settings
    (require 'aero-private)))

(provide 'aero-layers)

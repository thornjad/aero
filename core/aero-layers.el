;; -*- lexical-binding: t -*-
;; Aero layer system
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

;; TODO any possible way to do this async?
(defun aero/load-layers ()
	"Load all layers in aero-layer-directory"
	(let ((layer-dirs (directory-files-recursively
										 aero-layer-directory "layer\.el")))
		(dolist (layer layer-dirs)
			(load (symbol-name layer)))))

(provide 'aero-layers)

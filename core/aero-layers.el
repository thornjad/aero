;; -*- lexical-binding: t -*-
;; Aero layer system
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
;;
;; License: GPLv3

;; TODO any possible way to do this async?
(defun aero/load-layers ()
	"Load all layers in aero-layer-directory"
	(let ((layer-dirs (directory-files-recursively
										 aero-layer-directory "layer\.el"))))
	(dolist (layer layer-dirs)
		(load (symbol-name layer))))

(provide 'aero-layers)

;;; -*- lexical-binding: t -*-
;;;
;;; Define and add load paths

(defun add-to-load-path (dir)
	(add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
	(when (file-exists-p dir)
		(add-to-load-path dir)))

(defmacro def-path! (name base dir)
	"Define a directory constant in the `dir' directory of `base'"
	(let ((dir-name (intern (concat "aero-" (symbol-name name) "-directory")))
				(dir-base (intern (concat "aero-" (symbol-name base) "-directory"))))
		`(defconst ,dir-name
			 (expand-file-name (concat ,dir-base ,dir)))))

(defvar aero-start-directory
	user-emacs-directory)

(def-path! core start "core/")
(def-path! layer core "layers/")
(def-path! private start "private/")
(def-path! cache start ".cache/")
(def-path! autosave cache "auto-save/")
(def-path! test start "test/")

(defconst user-home-directory
	(expand-file-name "~/"))
(defconst pcache-directory
	(concat aero-cache-directory "pcache/"))
(unless (file-exists-p aero-cache-directory)
	(make-directory aero-cache-directory))

(mapc 'add-to-load-path
			`(,aero-core-directory
				,(concat aero-core-directory "libs/")
				,(concat aero-core-directory "libs/aero-theme/")))

(add-to-list 'custom-theme-load-path (concat aero-core-directory
																						 "libs/aero-theme/"))

(provide 'core-load-paths)

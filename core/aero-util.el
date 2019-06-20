;; -*- lexical-binding: t -*-
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
;;
;; Commentary:
;;
;; A home for non-interactive utilities
;;
;; Code:

(eval-when-compile
  (require 'cl-lib))


;; async

(use-package async :ensure t
	:config
	;; async dired operations
	(autoload 'dired-async-mode "dired-async.el" nil t)
	(dired-async-mode 1)
	;; async byte compilation
	(async-bytecomp-package-mode 1))


;; loading

(defmacro aero/require-and-exec! (feature &rest body)
  "Require the feature and execute body if it was successfull loaded."
  (declare (indent defun))
  `(if (require ,feature nil 'noerror)
       (progn ,@body)
     (message (format "%s not loaded" ,feature))))

(defmacro aero/load-and-exec! (file &rest body)
  "Load the file and execute body if it was successfull loaded."
  (declare (indent 1))
  `(if (load ,file t)
       (progn ,@body)
     (message (format "%s not loaded" ,file))))

;; https://github.com/syl20bnr/spacemacs/issues/8414
(defun aero/recompile-elpa (arg)
  "Compile or recompile packages in elpa directory, if needed, that is
    if the corresponding .elc file is either missing or outdated.

      If ARG is non-nil, also recompile every `.el' file, regardless of date.

      Useful if you switch Emacs versions."
  (interactive "P")
  ;; First argument must be 0 (not nil) to get missing .elc files rebuilt.
  ;; Bonus: Optionally force recompilation with universal ARG
  (byte-recompile-directory package-user-dir 0 arg))



;; modes and hooks

(defun aero/in-mode-p (mode)
  (eq major-mode mode))

(defun aero/run-prog-mode-hooks ()
	"Run `prog-mode-hook', useful for modes that don't derive from `prog-mode' but
should"
	(run-hooks 'prog-mode-hook))

(defun aero/run-text-mode-hooks ()
	"Run `text-mode-hook', useful for modes that don't derive from `text-mode' but
should"
	(run-hooks 'text-mode-hook))

(defmacro aero/add-hook! (hook &rest body)
	"Nicer add-hooking that prevents writing lambdas explicitely. Add a lamdba
containing BODY to hook HOOK."
	(declare (indent 1))
	`(add-hook ,hook
						 (lambda () ,@body)))

(defmacro aero/add-transient-hook! (hook func &optional fname)
  "Add transient hook by hook FUNC to HOOK.
Transient hooks are ephemeral hooks that vanishes when executed.
If FUNC is a lambda you must give it a name with FNAME. "
  (declare (indent 1))
  (let ((hfunc (intern (format "aero//transient-hook-%s"
                               (if fname fname func))))
        result)
    (setq result
          (append (when fname
                    `((fset ',fname (lambda (&rest _) (funcall #',func)))))
                  `((fset ',hfunc (lambda (&rest _)
                                    ,(if fname (list fname) (list func))
                                    ,(if (functionp hook)
                                         `(advice-remove ',hook ',hfunc)
                                       `(remove-hook ',hook ',hfunc))
                                    (fset ',hfunc 'ignore)
                                    ,(when fname `(fset ',fname 'ignore)))))
                  (if (functionp hook)
                      `((advice-add ',hook :before ',hfunc))
                    `((add-hook ',hook ',hfunc)))))
    (push 'progn result)))

(defvar aero/before-kill-hook
  '(recentf-save-list
    bookmark-save)
  "Functions to be executed by `SAVE-KILL-EMACS'")


;; controlling

(defun aero/modified-buffers-p ()
  "Returns first modified buffer or nil if there is none."
  (cl-loop for b in (buffer-list)
           when (and (buffer-live-p b)
                     (buffer-modified-p b)
                     (buffer-file-name b))
           return b))


;; logging

(defun aero/log-error (msg &rest args)
  "Display MSG as an error message in `*Messages*' buffer"
  (let ((msg (apply 'format msg args)))
    (message "(aero) Error: %s" msg)))

(defun aero/log-warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'"
  (let ((msg (apply 'format msg args)))
    (message "(aero) Warning: %s" msg)))

(defun aero/log-info (msg &rest args)
	"Display MSG as an info message in buffer `*Messages'"
	(let ((msg (apply 'format msg args)))
		(message "(aero) Info: %s" msg)))

(defun aero/echo (msg &rest args)
  "Display MSG in echo-area without logging it in `*Messages' buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))


;; files, buffers, windows

(defun aero|move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window, using the aero numbering. follow-focus-p controls
   whether focus moves to new window (with buffer), or stays on current"
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))


;; general nice functions

(defmacro p (&rest body)
  "Create anonymous predicate"
  `(lambda (x)
     ,@body))

(defun system-is-mac ()
  (eq system-type 'darwin))
(defun system-is-linux ()
  (eq system-type 'gnu/linux))
(defun system-is-mswindows ()
  (eq system-type 'windows-nt))
(defun window-system-is-mac ()
  (memq (window-system) '(mac ns)))

;; http://stackoverflow.com/questions/11847547/emacs-regexp-count-occurrences
(defun how-many-str (regexp str)
  (cl-loop with start = 0
					 for count from 0
					 while (string-match regexp str start)
					 do (setq start (match-end 0))
					 finally return count))

(defun iso-timestamp ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

(defun comment-date ()
  (let ((time (format-time-string "[%Y-%m-%d %H:%M:%S]")))
    (format "%s %s\n%s\t" comment-start time comment-start)))

(provide 'aero-util)

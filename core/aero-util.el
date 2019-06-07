;; -*- lexical-binding: t -*-
;; Utilities
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
;;
;; License: GPLv3

(use-package cl-lib :ensure t)


;; loading

(defmacro aero/require-and-exec! (feature &rest body)
  "Require the feature and execute body if it was successfull loaded."
  (declare (indent defun))
  `(if (require ,feature nil 'noerror)
       (progn ,@body)
     (message (format "%s not loaded" ,feature))))

(defmacro aero/load-and-exec! (file &optional &rest body)
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

(defun aero/save-kill-emacs ()
  (interactive)
  (mapc (lambda (f) (ignore-errors (funcall f)))
        aero/before-kill-hook)
  (if (modified-buffers-p)
      (progn
        (when (not (eq window-system 'x))
          (x-initialize-window-system))
        (select-frame (make-frame-on-display (getenv "DISPLAY") '((window-system . x))))
        (save-some-buffers)
        (if (yes-or-no-p "Kill Emacs? ")
            (kill-emacs)))
    (kill-emacs)))

;; https://sachachua.com/blog/2006/09/emacs-changing-the-font-size-on-the-fly/
(defun aero/increase-font-size ()
  (interactive)
  (set-face-attribute
	 'default
   nil
   :height
   (ceiling (* 1.10
               (face-attribute 'default :height)))))
(defun aero/decrease-font-size ()
  (interactive)
  (set-face-attribute
	 'default
   nil
   :height
   (floor (* 0.9
             (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'aero/increase-font-size)
(global-set-key (kbd "C--") 'aero/decrease-font-size)


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


;; files and buffers
(defun aero/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun aero/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun aero/buffer-too-big-p ()
	"Is buffer longer than 5000"
  (> (buffer-size) (* 5000 80)))

(defun aero/file-too-big-p (file)
	"Is file larger than 5000 lines"
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

(defun aero/open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (shell-command (format "open \"%s\"" file-path))
      (message "No file associated to this buffer."))))

(defun aero/finder-here ()
	"Open macOS Finder here"
  (interactive)
	(when (system-is-mac)
		(let* ((dir default-directory)
					 (scr (format " do shell script \"open %s\"\n" dir)))
			(do-applescript scr))))

;; adapted from
;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
(defun aero/kill-diff-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (member (buffer-local-value 'major-mode buffer)
                        '(diff-mode magit-diff-mode magit-process-mode))
            (kill-buffer buffer)))
        (buffer-list)))


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

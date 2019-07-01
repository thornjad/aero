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
;; A home for interactive utilities
;;
;; Code:

(require 'aero-files)
(require 'cl-lib)

;; program-wide

(defun aero/save-kill-emacs ()
  (interactive)
  (mapc (lambda (f) (ignore-errors (funcall f)))
        '(recentf-save-list bookmark-save))
  (if (modified-buffers-p)
      (progn
        (when (not (eq window-system 'x))
          (x-initialize-window-system))
        (select-frame (make-frame-on-display (getenv "DISPLAY") '((window-system . x))))
        (save-some-buffers)
        (if (yes-or-no-p "Kill Emacs? ")
            (kill-emacs)))
    (kill-emacs)))

;; https://github.com/syl20bnr/spacemacs/issues/8414
(defun aero/recompile-elpa (arg)
  "Compile or recompile packages in elpa directory, if needed, that is if the
    corresponding .elc file is either missing or outdated. If ARG is non-nil,
    also recompile every `.el' file, regardless of date. Useful if you switch
    Emacs versions."
  (interactive "P")
  ;; First argument must be 0 (not nil) to get missing .elc files rebuilt.
  ;; Bonus: Optionally force recompilation with universal ARG
  (when arg
    (seq-do
     (lambda (fname)
       (when (file-exists-p fname)
         (delete-file fname)))
     (directory-files-recursively user-emacs-directory "\\.elc$" t)))
  (byte-recompile-directory package-user-dir 0 arg))

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

(defun aero/apologize-to-emacs ()
	"Apologize for smacking emacs (stop debug on quit). Use this after smacking
emacs with sigusr2"
	(interactive)
	(toggle-debug-on-quit))

(defun aero/thornlog ()
	"Personal persistent log"
	(interactive)
	(find-file (expand-file-name "~/doc/thornlog.org")))

(defun aero/open-tweaks ()
  "Open editor tweaks file"
  (interactive)
  (find-file (format "%s/%s" aero-layer-directory "aero-tweaks.el")))

(defun aero/reload-tweaks ()
  "Reload tweaks file. This may not be a full load, depending on contents"
  (interactive)
  (load-file (format "%s/%s" aero-layer-directory "aero-tweaks.el")))

(defun aero/stop-auto-revert-buffers ()
	(cancel-function-timers 'auto-revert-buffers))


;; buffers, windows

(defun aero/move-buffer-to-window ()
	"Interactive move buffer to window"
	(interactive)
	(aero|move-buffer-to-window (read-string "Move to:") t))

(defun aero/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun switch-to-messages-buffer ()
	(interactive)
	(switch-to-buffer "*Messages*"))

(defun switch-to-scratch-buffer ()
	(interactive)
	(switch-to-buffer "*Scratch*"))

;; from spacemacs
(defun aero/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (cl-destructuring-bind
   (buf start pos)
   (or (cl-find (window-buffer window) (window-prev-buffers)
                :key #'car :test-not #'eq)
       (list (other-buffer) nil nil ))
   (set-window-buffer-start-and-point window buf start pos)))

(defun aero/alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))


;;; the rest

(defun shrug ()
	(interactive)
	(insert "¯\\_(ツ)_/¯"))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'aero-lib)

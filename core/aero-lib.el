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


;; program-wide

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

(defun aero/apologize-to-emacs ()
	"Apologize for smacking emacs (stop debug on quit). Use this after smacking
emacs with sigusr2"
	(interactive)
	(toggle-debug-on-quit))

(defun aero/thornlog ()
	"Personal persistent log"
	(interactive)
	(find-file "~/doc/thornlog.org"))

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


;;; the rest

(defun shrug ()
	(interactive)
	(insert "¯\\_(ツ)_/¯"))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(provide 'aero-lib)

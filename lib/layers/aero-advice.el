;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2023 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs


;; lib functions

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun aero/advice-disable-subword (orig-fun &rest args)
  "Disable `subword-mode' around the given function."
  (let ((original-mode subword-mode))
    (subword-mode -1)
    (apply orig-fun args)
    (subword-mode original-mode)))

(defun aero/advice-no-message (fn &rest args)
  "Advise function FN with ARGS not to message at all."
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply fn args)))


;; misc advice

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defun aero/advice-elisp-get-fnsym-args-string (fn sym &rest args)
  "If SYM is a function, append its docstring."
  (concat
   (apply fn sym args)
   (let ((doc (and (fboundp sym) (documentation sym 'raw))))
     (and doc
          (stringp doc)
          (not (string= "" doc))
          (concat "\n\n" (propertize doc 'face 'italic))))))
(advice-add 'elisp-get-fnsym-args-string :around #'aero/advice-elisp-get-fnsym-args-string)

(define-advice comment-indent-new-line (:after (&optional soft) at-least-one-space)
  "Ensure that at least one space is added after the comment-start."
  (let ((start (regexp-quote comment-start)))
    (when (and (nth 4 (syntax-ppss))
               (looking-back start (+ (point) (length start)))
               (not (looking-back " "  (+ (point) 1))))
      (insert " "))))

;; Don't kill scratch buffer, just bury it if something tries to
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Don't kill my scratch!"
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

(provide 'aero-advice)

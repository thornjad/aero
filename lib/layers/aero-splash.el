;;; Custom splash screen -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Jade Michael Thornton
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;;
;; The software is provided "as is" and the author disclaims all warranties with
;; regard to this software including all implied warranties of merchantability
;; and fitness. In no event shall the author be liable for any special, direct,
;; indirect, or consequential damages or any damages whatsoever resulting from
;; loss of use, data or profits, whether in an action of contract, negligence or
;; other tortious action, arising out of or in connection with the use or
;; performance of this software.

;;; Commentary:

;;; Code:

(require 'cl-lib)

;; Pacify the compiler
(defvar package-activated-list)

(defgroup aero-splash nil
  "Simple splash screen."
  :group 'applications)

(defconst aero-splash-buffer-name "*splash*"
  "Aero splash buffer name.")


;;; Primary function

(defun aero-splash ()
  "Aero splash screen."
  (interactive)
  (let* ((splash-buffer (get-buffer-create aero-splash-buffer-name))
         (recover-session (and auto-save-list-file-prefix
                               (file-directory-p (file-name-directory
                                                  auto-save-list-file-prefix))))
         (height         (- (window-body-height nil) 7))
         (width          (window-body-width nil))
         (padding-center (- (/ height 2) 1))
         (padding-bottom (- height padding-center 3)))

    (with-current-buffer splash-buffer
      (read-only-mode -1) (erase-buffer)

      ;; Buffer local settings
      (setq-local mode-line-format nil)
      (setq cursor-type nil)
      (setq vertical-scroll-bar nil)
      (setq horizontal-scroll-bar nil)
      (setq fill-column width)
      (face-remap-add-relative 'link :underline nil)

      ;; Vertical padding to center
      (insert-char ?\n padding-center)

      ;; Central text
      (insert-text-button " Aero Emacs "
                          'action (lambda (_) (browse-url "https://gitlab.com/thornjad/aero"))
                          'help-echo "View source"
                          'follow-link t)
      (center-line) (insert "\n")
      (insert (concat "Built on GNU Emacs version "
                      (format "%d.%d" emacs-major-version emacs-minor-version)))
      (center-line) (insert "\n")
      (insert (propertize "finally, a good fucking editor" 'face 'shadow))
      (center-line)

      ;; Vertical padding to bottom
      (insert-char ?\n padding-bottom)

      ;; Recover session button
      (when recover-session
        (delete-char -2)
        (insert-text-button " [Recover session] "
                            'action (lambda (_) (call-interactively 'recover-session))
                            'help-echo "Recover previous session"
                            'face 'warning
                            'follow-link t)
        (center-line) (insert "\n") (insert "\n"))


      (insert (aero-splash--init-info))
      ;; (insert (propertize
      ;;          "GNU Emacs comes with ABSOLUTELY NO WARRANTY" 'face 'shadow))
      (center-line) (insert "\n")
      ;; (insert (propertize
      ;;          "Copyright (C) 2020 Free Software Foundation, Inc." 'face 'shadow))
      (center-line) (insert "\n")

      (goto-char 0)
      (read-only-mode t)
      (display-buffer-same-window splash-buffer nil))))

(defun aero-splash--init-info ()
  "Insert init info."
  (propertize
   (if (bound-and-true-p package-alist)
       (format "%d packages loaded in %s"
               (length package-activated-list) (emacs-init-time))
     (if (and (boundp 'straight--profile-cache) (hash-table-p straight--profile-cache))
         (format "%d packages loaded in %s"
                 (hash-table-size straight--profile-cache) (emacs-init-time))
       (format "Emacs started in %s" (emacs-init-time))))
   'face 'font-lock-comment-face))

(defun aero-splash-kill-splash ()
  "Kill the splash screen."
  (interactive)
  (when (get-buffer aero-splash-buffer-name) (kill-buffer aero-splash-buffer-name)))


;;; Activate on startup

;; Install hook after frame parameters have been applied and only if no option
;; on the command line
(when (and (not (member "-no-splash"  command-line-args))
           (not (member "--file"      command-line-args))
           (not (member "--insert"    command-line-args))
           (not (member "--find-file" command-line-args))
           (not inhibit-startup-screen))
  (progn
    (add-hook 'window-setup-hook #'aero-splash-mode)
    (setq inhibit-startup-screen t
          inhibit-startup-message t
          inhibit-startup-echo-area-message t)))

(provide 'aero-splash)

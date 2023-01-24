;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton
;;
;; This file is not part of GNU Emacs
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
;;
;;; Commentary:
;;
;; It looks a lot like doom modeline, but that's really more like convergent evolution than copying.
;; Though I will admit I more or less stole that sweet bar on the left side of the line.
;;
;;; Code:

(defgroup aero/modeline nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defvar aero/modeline--bar-active nil)
(defvar aero/modeline-bar--inactive nil)

(defvar aero/modeline-height 30)
(defvar aero/modeline-bar-width 5)

(defface aero/modeline-status-grayed-out '((t (:inherit (font-lock-doc-face) :slant italic)))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-info '((t (:inherit (font-lock-keyword-face) :slant italic)))
  "Face used for generic status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-success '((t (:inherit (success) :slant italic)))
  "Face used for success status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-warning '((t (:inherit (warning) :slant italic)))
  "Face for warning status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-error '((t (:inherit (error) :slant italic)))
  "Face for error stauts indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-unimportant '((t (:inherit (font-lock-doc-face))))
  "Face used for less important mode-line elements."
  :group 'aero/modeline)

(defface aero/modeline-modified '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-not-modified '((t (:inherit (success))))
  "Face used for the 'not modified' indicator symbol in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-read-only '((t (:inherit (warning))))
  "Face used for the 'buffer read-only' indicator symbol in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-remote '((t (:inherit (font-lock-keyword-face :weight bold))))
  "Face used for the 'not modified' indicator symbol in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-evil-normal '((t (:inherit (font-lock-keyword-face))))
  "Face used for Normal Evil state message."
  :group 'aero/modeline)
(defface aero/modeline-evil-insert '((t (:inherit (font-lock-keyword-face))))
  "Face used for Insert Evil state message."
  :group 'aero/modeline)
(defface aero/modeline-evil-visual '((t (:inherit (font-lock-keyword-face))))
  "Face used for Visual Evil state message."
  :group 'aero/modeline)
(defface aero/modeline-evil-operator '((t (:inherit (font-lock-keyword-face))))
  "Face used for Visual Evil state message."
  :group 'aero/modeline)
(defface aero/modeline-evil-motion '((t (:inherit (font-lock-keyword-face))))
  "Face used for Visual Evil state message."
  :group 'aero/modeline)
(defface aero/modeline-evil-replace '((t (:inherit (font-lock-keyword-face))))
  "Face used for Replace Evil state message."
  :group 'aero/modeline)
(defface aero/modeline-evil-emacs '((t (:inherit (font-lock-keyword-face))))
  "Face used for Emacs Evil state message."
  :group 'aero/modeline)

(defface aero/modeline-major-mode-active '((t (:inherit mode-line-buffer-id)))
  "Face used for major mode."
  :group 'aero/modeline)

(defface aero/modeline-git-branch '((t (:slant italic :bold t)))
  "Used for Git branch name."
  :group 'aero/modeline)

(defface aero/modeline-bar '((t (:background unspecified)))
  "Style of the bar on the modeline."
  :group 'aero/modeline)
(defface aero/modeline-bar-inactive '((t (:background unspecified)))
  "Style of the bar on the inactive modeline."
  :group 'aero/modeline)

;; TODO could this be memoized?
(defun aero-info-line-format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned
respectively."
  (let ((reserve (length right)))
    (concat left " " (propertize
                      " " 'display
                      `((space :align-to
                          (- (+ right right-fringe right-margin)
                             ,(+ reserve (if (display-graphic-p) 1 2))))))
            right)))

(defvar aero/modeline--active-window nil)
(defun aero/modeline--get-active-window (&optional frame)
  "Get the current window, but exclude child windows."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))
(defun aero/modeline--set-selected-window (&rest _)
  "Set `aero/modeline--active-window' to the correct window."
  (let ((win (aero/modeline--get-active-window)))
    (setq aero/modeline--active-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))
(add-hook 'pre-redisplay-functions #'aero/modeline--set-selected-window)
(defun aero/modeline--active-p ()
  "Return whether mode-line is active."
  (and aero/modeline--active-window
       (eq (aero/modeline--get-active-window) aero/modeline--active-window)))

;;; Segments

(defun aero/modeline-segment-evil-state ()
  "Display current evil state. Requires function `evil-mode'."
  (when (require 'evil nil 'noerror)
    (declare-function evil-state-property "evil")
    (defvar evil-state)
    (let ((state (evil-state-property evil-state :tag t)))
      (cond
       ((functionp state) (propertize (funcall state) 'face 'aero/modeline-evil-visual))
       ((string= state " <N> ") (propertize state 'face 'aero/modeline-evil-normal))
       ((string= state " <I> ") (propertize state 'face 'aero/modeline-evil-insert))
       ((string= state " <R> ") (propertize state 'face 'aero/modeline-evil-replace))
       ((string= state " <O> ") (propertize state 'face 'aero/modeline-evil-operator))
       ((string= state " <M> ") (propertize state 'face 'aero/modeline-evil-motion))
       ((string= state " <E> ") (propertize state 'face 'aero/modeline-evil-emacs))
       (t state)))))

(defun aero/modeline-segment-modified ()
  "Displays a color-coded buffer modification indicator in the mode-line."
  (cond
   ((and buffer-read-only (buffer-file-name))  ;; read-only
    (propertize "" 'face `(:inherit aero/modeline-read-only :height 0.6)))
   ((string-match-p "\\*.*\\*" (buffer-name))  ;; special buffer
    (propertize "" 'face `(:inherit aero/modeline-read-only :height 0.6)))
   ((buffer-modified-p)  ;; modified
    (propertize "✎" 'face `(:inherit aero/modeline-modified :height 0.65)))
   (t  ;; not modified
    (propertize "" 'face `(:inherit aero/modeline-not-modified :height 0.6)))))

(defun aero/modeline-segment-git-state ()
  "Displays the current branch and status from Git.

Only Git is supported because I'm not an animal."
  (when (and vc-mode buffer-file-name)
    (let ((state (vc-state (file-local-name buffer-file-name)))
          (str (if vc-display-status
                   (substring vc-mode 5)
                 "")))
      (concat
       " "
       (propertize
        (let ((max 13))
          (if (> (length str) max)
              (concat (substring str 0 (- max 3)) "…") ; substring 3 less than length limit
            str))
        'mouse-face 'mode-line-highlight
        'face 'aero/modeline-git-branch)
       " "))))

(defun aero/modeline-segment-remote ()
  "Displays a symbol if buffer is remote"
  (when-let* ((filename (buffer-file-name))
              (host (file-remote-p filename 'host)))
    (concat " @" (propertize host 'face 'aero/modeline-remote) " ")))

(defun aero/modeline-segment-buffer-name ()
  "Displays the name and size of the current buffer in the mode-line."
  (concat (propertize "%b" 'face 'mode-line-buffer-id) " "))

(defun aero/modeline-segment-size-and-position ()
  "Displays the current cursor position in the mode-line."
  (concat "(%I) %l:%C %o%%"
          (when (use-region-p)
            (concat
             "  (" (number-to-string (count-lines (point) (mark)))
             ":" (number-to-string (abs (- (point) (mark))))
             ")"))
          "  "))

(defun aero/modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (list mode-line-process "  ")))

(defun aero/modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (format " %s " (format-mode-line mode-name)))

(defun aero/modeline-create-bar-image (face width height)
  "Create the bar image.
Use FACE1 for the bar, FACE2 for the background.
WIDTH and HEIGHT are the image size in pixels."
  (when (and (display-graphic-p)
             (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (let ((color (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (make-string (* width height) ?1)
                  "\n")
          'pbm t :foreground color :ascent 'center))))))

(defun aero/modeline-segment-bar ()
  "The bar, also determines modeline height (in GUI)."
  (let ((width aero/modeline-bar-width)
        (height aero/modeline-height))
    (if (aero/modeline--active-p)
        (aero/modeline-create-bar-image 'aero/modeline-bar width height)
      (aero/modeline-create-bar-image 'aero/modeline-bar-inactive width height))))

;;; Activation function

;; Store the default mode-line format
(defvar aero/modeline--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode aero/modeline-mode
  "Toggle aero/modeline on or off."
  :group 'aero/modeline
  :global t
  :lighter nil
  (progn
    ;; Set the new mode-line-format
    (setq-default mode-line-format
                  '((:eval
                     (aero-info-line-format
                      ;; Left
                      (format-mode-line
                       '((:eval (aero/modeline-segment-bar))
                         (:eval (aero/modeline-segment-evil-state))
                         " " (:eval (aero/modeline-segment-modified)) " "
                         (:eval (aero/modeline-segment-buffer-name))
                         (:eval (aero/modeline-segment-size-and-position))))

                      ;; Right
                      (format-mode-line
                       '((:eval (aero/modeline-segment-process))
                         (:eval (aero/modeline-segment-git-state))
                         (:eval (aero/modeline-segment-remote))
                         (:eval (aero/modeline-segment-major-mode))
                         ))))))))

;;;###autoload
(define-globalized-minor-mode aero/modeline-global-mode aero/modeline-mode
  (lambda () (aero/modeline-mode 1)))

(provide 'aero-modeline)

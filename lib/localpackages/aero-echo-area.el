;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023-2024 Jade Michael Thornton
;;
;; Based on echo-bar by Adam Tillov, copyright 2021-2022
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
;;; Code:

(require 'timer)
(require 'overlay)
(require 'battery)

(defvar aero/echo-area-text nil
  "The text currently displayed in the echo bar.")

(defvar aero/echo-area-overlays nil
  "List of overlays displaying the echo bar contents.")

(defvar aero/echo-area--previous-text nil
  "The text previously displayed in the echo bar, checked against for updates.")

(defun aero/echo-area--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (string-pixel-width str)))
    (+ (/ len width) (if (zerop (% len width)) 0 1))))

(defun aero/echo-area-set-text (text)
  "Set the text displayed by the echo bar to TEXT."
  (let* ((wid (+ (aero/echo-area--str-len text) 1
                 (if (and (display-graphic-p)
                          (> (nth 1 (window-fringes)) 0)
                          (not overflow-newline-into-fringe))
                     1
                   0)))
         ;; Maximum length for the echo area message before wrap to next line
         (max-len (- (frame-width) wid 5))
         ;; Align the text to the correct width to make it right aligned
         (spc (propertize " " 'cursor 1 'display
                          `(space :align-to (- right-fringe ,wid)))))

    (setq aero/echo-area-text (concat spc text))

    ;; Add the correct text to each echo bar overlay
    (when (not (string= aero/echo-area-text aero/echo-area--previous-text))
      (dolist (o aero/echo-area-overlays)
        (when (overlay-buffer o)

          (with-current-buffer (overlay-buffer o)
            ;; Wrap the text to the next line if the echo bar text is too long
            (if (> (mod (point-max) (frame-width)) max-len)
                (overlay-put o 'after-string (concat "\n" aero/echo-area-text))
              (overlay-put o 'after-string aero/echo-area-text))))))


    (with-current-buffer " *Minibuf-0*"
      ;; If the minibuffer is not Minibuf-0, then the user is using the minibuffer
      (when (eq (current-buffer) (window-buffer (minibuffer-window)))

        ;; Don't override existing text in minibuffer, such as ispell
        (when (get-text-property (point-min) 'aero/echo-area)
          (delete-region (point-min) (point-max)))
        (when (= (point-min) (point-max))
          ;; Display the full text in Minibuf-0, as overlays don't show up
          (insert (propertize aero/echo-area-text 'aero/echo-area t)))))))

(defun aero/echo-area--new-overlay ()
  "Add new aero/echo-area overlay."
  (let ((new-overlay (make-overlay (point-max) (point-max) nil t t)))
    (push new-overlay aero/echo-area-overlays)
    new-overlay))

(defun aero/echo-area-update ()
  "Get new text to be displayed from `aero/echo-area-default-function`."
  (when aero/echo-area-mode
    (aero/echo-area-set-text (aero/echo-area-format-function))))

(defun aero/echo-area-format-function ()
  "Return the text to be displayed in the echo area."
  (let ((status (ignore-errors (funcall battery-status-function)))
        (percent (when status (round (string-to-number (battery-format "%p" status)))))
        (power-method (when status (battery-format "%L" status))))
    (format "%s%% %s  |  %s"
            (or percent "")
            (if (string= power-method "AC") "[charging]" "")
            (format-time-string "%A, %d %b — %R"))))

(defun aero/echo-area-enable ()
  "Turn on the echo bar."
  (interactive)
  ;; Disable any existing echo bar to remove conflicts
  (aero/echo-area-disable)

  ;; Create overlays in each echo area buffer. Use `get-buffer-create' to make
  ;; sure that the buffer is created even if no messages were outputted before
  (dolist (buf (mapcar #'get-buffer-create '(" *Echo Area 0*" " *Echo Area 1*")))
    (with-current-buffer buf
      (remove-overlays (point-min) (point-max))
      (aero/echo-area--new-overlay)))

  (run-with-timer 0 1 'aero/echo-area-update)
  (aero/echo-area-update))

(defun aero/echo-area-disable ()
  "Turn off the echo bar."
  (interactive)

  (mapc 'delete-overlay aero/echo-area-overlays)
  (setq aero/echo-area-overlays nil)

  ;; Remove text from Minibuf-0
  (with-current-buffer (window-buffer (minibuffer-window))
    (delete-region (point-min) (point-max)))

  ;; Cancel the update timer
  (cancel-function-timers #'aero/echo-area-update))

;;;###autoload
(define-minor-mode aero/echo-area-mode
  "Display text at the end of the echo area."
  :global t
  (if aero/echo-area-mode
      (aero/echo-area-enable)
    (aero/echo-area-disable)))

(provide 'aero-echo-area)
;;; aero-echo-area.el ends here

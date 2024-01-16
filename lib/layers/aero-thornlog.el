;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2024 Jade Michael Thornton
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
;; TODO automatically archive after 30 or 60 days
;;
;;; Code:

(require 'aero-prelude)
(require 'outline)

(defun new-day ()
  "Create a new entry for today, if one isn't already present."
  (interactive)
  (cond
   ((not (string-match "thornlog/log\\.md" (buffer-file-name)))
    (message "Not in Thornlog file"))
   ((today t) (message "Entry for today already present"))
   (t (progn
        (new-day-insert)
        (when (require 'evil nil t)
          (evil-scroll-line-to-center (line-number-at-pos)))))))

;; List of things we expand inside the templated-section of this file.
;; The pairs are "regexp" + "replacement" which is invoked via "apply".
(setq new-day-template-variables
      '(("YYYY"        . (format-time-string "%Y"))
        ("MM"          . (format-time-string "%m"))
        ("DD"          . (format-time-string "%d"))
        ("MONTH"       . (format-time-string "%B"))
        ("MONTH3"      . (format-time-string "%b"))
        ("DAY"         . (format-time-string "%A"))
        ("DAY3"        . (format-time-string "%a"))
        ("HOUR"        . (format-time-string "%H"))
        ("MINUTE"      . (format-time-string "%M"))
        ("TEMPLATE"    . (format-time-string "%A, %B %d (%Y-%m-%d)"))
        (":noexport:"  . (format ""))))

(defun goto-last-day ()
  "Move point to the most recent entry."
  (setf (point) (point-max))
  (re-search-backward "^# [[:alpha:]]+, [[:alpha:]]" nil t))

(defun last-day-date-string ()
  "Get the date of the most recent entry."
  (save-excursion
    (goto-last-day)
    (let ((line (thing-at-point 'line t)))
      (string-match "(\\([0-9]+-[0-9]+-[0-9]+\\))" line)
      (match-string 1 line))))

(defun last-day-was-last-workday-p ()
  "Returns true if the most recent entry was the previous workday."
  (let ((last-workday
         (string-to-number (if (string= (day-of-week) "Monday")
                               (human-date "last Friday" t)
                             (human-date "yesterday" t)))))
    (string= (format-time-string "%Y-%m-%d" last-workday)
             (last-day-date-string))))

(defun last-day ()
  "Visit the most recent entry, not necessarily today."
  (interactive)
  (outline-hide-sublevels 1)
  (goto-last-day)
  (outline-show-subtree)
  (when (require 'evil nil t)
    (evil-scroll-line-to-center (line-number-at-pos))))

(defun last-days-goals ()
  "Get goals from last-day."
  (save-excursion
    (last-day)
    (re-search-forward "## Goals" nil t)
    (forward-line 1)
    (let ((start (point)))
      (re-search-forward "## Notes" nil t)
      (beginning-of-line)
      (backward-char 1)
      (let ((end (point)))
        (buffer-substring start end)))))

(defun last-days-notes ()
  "Get notes from last-day."
  (save-excursion
    (last-day)
    (re-search-forward "## Notes" nil t)
    (forward-line 1)
    (let ((start (point)))
      (setf (point) (point-max))
      (backward-char 1)
      (buffer-substring start (point)))))

(defun get-last-entry-day ()
  "Find the last entry's date and return the corresponding day of the week."
  (save-excursion
    (goto-char (point-max)) ; Go to the end of the buffer to start searching.
    (when (re-search-backward "^# \\([A-Za-z]+\\), \\([A-Za-z]+ [0-9]+\\) (\\([0-9-]+\\))" nil t)
      (let ((date-str (match-string 3)))
        (format-time-string "%A" (date-to-time date-str))))))

(defvar aero/thornlog-template
  "# TEMPLATE

## Sync summary

### Since yesterday (tech design status, hotfixes, tough investigations, QA work, etc.)


### Today

### Achievements

### Blocked
∅

## Goals

## Notes

"
  "Template for a new day in the thornlog")

(defvar aero/thornlog-blocked-response-list
  '("none" "none" "none" "none" "none" "none" "none"
    "nothing" "nope" "nil" "zilch" "naught" "void" "n/a"
    "∅" "nada" "pas une chose" "無" "żadnych")
  "List of template responses for blocked, to be chosen randomly.

'none' is included multiple times so as to give it increased weight, it being the 'normal'
response. I'm too lazy to create a weights map or something, this is easier.")

(defun new-day-insert ()
  "Insert the contents of the template into the document, for a new day's work."
  (let ((text nil)
        (case-fold-search nil)) ; case-insensitive search
    (setf (point) (point-max))
    (save-excursion
      (setq text aero/thornlog-template)

      ;; Replace all our template-pairs
      (dolist (item new-day-template-variables)
        (setq text (replace-regexp-in-string (car item) (apply (cdr item)) text t)))
      ;; Fill in yesterday's status as a head start
      (when (last-day-was-last-workday-p)
        (setq text (replace-regexp-in-string
                    "## Goals\n"
                    (concat "## Goals\n" (last-days-goals))
                    text t))
        (setq text (replace-regexp-in-string
                    "## Notes\n"
                    (concat "## Notes\n" (last-days-notes))
                    text t)))

      ;; Adjust for arbitrary log gaps, such as weekends
      (let ((last-entry-day (get-last-entry-day)))
        (unless (string= (day-of-week) (day-after last-entry-day))
          (setq text (replace-regexp-in-string "### Since yesterday" (format "### Since %s" last-entry-day) text t))))

      ;; Put in a random blocked message
      (setq text (replace-regexp-in-string
                  "∅"
                  (rand-nth aero/thornlog-blocked-response-list)
                  text t))

      ;; Done, insert
      (insert text "\n"))
    (forward-line 1)
    (outline-hide-sublevels 1) ; collapse all entries
    (outline-show-subtree) ; expand current day

    ;; Expand previous day too
    (forward-line -1)
    (when (re-search-backward "^# .*" nil t)
      (goto-char (match-beginning 0))
      (outline-show-subtree))

    (setf (point) (point-max))
    (re-search-backward "### Since ")
    (forward-line 1)))

(defun today (&optional nomsg)
  "Visit today's entry, if it exists, message if NOMSG is nil."
  (interactive)
  (let ((pos nil))
    (save-excursion
      (setf (point) (point-min))
      (if (re-search-forward (format-time-string "^#.* (%Y-%m-%d)") nil t)
          (setq pos (point))
        (unless nomsg (message "No entry for today found."))))
    (if pos
        (progn
          (setf (point) pos)
          (beginning-of-line)
          (outline-hide-sublevels 1)
          (outline-show-subtree)
          (when (require 'evil nil t)
            (evil-scroll-line-to-center (line-number-at-pos)))
          t)
      (progn
        (setf (point) (point-max))
        nil))))

(defun aero/thornlog-today ()
  "Open thornlog and visit today."
  (interactive)
  (aero/thornlog-log)
  (today))

(defvar aero/thornlog-path
  (expand-file-name "~/Documents/thornlog/")
  "Location of the thornlog on this filesystem.")

(defun aero/thornlog-dir ()
  "Personal persistent log."
  (interactive)
  (declare-function deer "ranger.el")
  (when (require 'ranger nil t)
    (deer aero/thornlog-path)))

(defun aero/thornlog-log ()
  "Personal persistent log."
  (interactive)
  (find-file (expand-file-name "log.md" aero/thornlog-path)))

(defun aero/thornlog-notes ()
  "Personal notes file."
  (interactive)
  (find-file (expand-file-name "notes.md" aero/thornlog-path)))

(defun aero/thornlog-todo ()
  "Personal todo list."
  (interactive)
  (find-file (expand-file-name "todo.md" aero/thornlog-path)))

(provide 'aero-thornlog)

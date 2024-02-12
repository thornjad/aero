;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2021, 2024 Jade Michael Thornton
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

(require 'aero-prelude)

(defvar aero/org-eval-safe-list
  '(expand-file-name "~/doc/thornlog/")
  "Directories which will have their contents evaluated without prompting.")

(package! org :builtin
  :custom
  (org-insert-heading-respect-content t)
  (org-fold-catch-invisible-edits 'smart)
  (org-src-preserve-indentation t)
  (org-footnote-auto-adjust t)
  (org-footnote-section nil)
  (org-startup-with-inline-images t)
  (org-startup-indented t)
  (org-log-done 'time) ; log time when item is marked done
  (org-fontify-done-headline t) ; let theme strike out done items

  ;; re-scale images to 400px if no with attribute is set (see
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
  (org-image-actual-width '(400))

  ;; TODO org-modules, here are sacha's:
  ;; (setq org-modules '(org-habit
  ;;                     org-mouse
  ;;                     org-protocol
  ;;                     org-annotate-file
  ;;                     org-eval
  ;;                     org-expiry
  ;;                     org-interactive-query
  ;;                     org-collector
  ;;                     org-panel
  ;;                     org-screen
  ;;                     org-toc))
  ;; (eval-after-load 'org
  ;;   '(org-load-modules-maybe t))

  ;; all agenda files
  (org-agenda-files `(,(expand-file-name "todo.org" aero/thornlog-path)))

  (org-agenda-span 5) ; days to show at a time
  (org-agenda-tags-column -70) ; shift tags over
  (org-agenda-sticky nil) ; don't bury on close buffer
  (org-agenda-use-tag-inheritance t)
  (org-agenda-show-log t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-start-on-weekday 1) ; start week on Monday
  (org-agenda-window-setup 'current-window)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-start-day "-3d")
  (org-agenda-inhibit-startup t) ; skip unnecessary loading time
  (org-priority-faces '((?A . error) (?B . warning) (?C . success) (?D . org-priority)))
  (org-archive-location (concat (expand-file-name "archive/archive.org" aero/thornlog-path) "::* From %s"))

  :config
  (aero-leader-def
    "oT" '(:ignore t :wk "time")
    "oTt" 'org-time-stamp
    "oTd" 'insert-todays-timestamp-at-entry-end
    "ot" 'org-todo
    "oA" 'org-archive-subtree-default
    "oa" '(:ignore t :wk "agenda")
    "oaa" 'aero/org-agenda-list
    "oat" 'aero/org-agenda-todo
    "oay" 'org-agenda-todo-yesterday
    "oax" 'aero/org-agenda-done
    "oaF" 'aero/org-agenda-done-and-followup
    "oaN" 'aero/org-agenda-new
    "oap" '(:ignore t :wk "priority")
    "op" '(:ignore t :wk "priority")
    "opp" 'org-priority
    "opu" 'org-priority-up
    "opd" 'org-priority-down
    "ops" 'org-priority-show
    "on" 'org-forward-heading-same-level
    "oN" 'org-backward-heading-same-level
    "oe" '(:ignore t :wk "org edit")
    "oet" '(:ignore t :wk "org table")
    "oets" 'org-table-sort-lines
    "oi" '(:ignore t :wk "insert")
    "oil" '(org-insert-link :wk "link")
    "oid" '(org-insert-drawer :wk "drawer")
    "oc" '(:ignore t :wk "cell")
    "occ" '(org-babel-execute-src-block :wk "exec cell")
    "oh" '(outline-hide-body :wk "hide all")
    "oS" '(outline-show-all :wk "show all")
    "os" 'org-sort-entries)

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  ;; start with all levels collapsed
  (add-hook 'org-mode-hook #'org-hide-block-all)

  ;; Show agenda when Emacs is idle for 10 minutes, from
  ;; https://sachachua.com/dotemacs/index.html#idle-timer
  (run-with-idle-timer 600 t 'jump-to-org-agenda))

;; Org-mode UI improvements
(package! org-modern "minad/org-modern"
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda)))

;; Use evil in org, particularly in org-agenda. Also unblocks using aero-leader chords. See
;; https://github.com/Somelauw/evil-org-mode for a list of available commands
(package! evil-org-mode "Somelauw/evil-org-mode"
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :init
  (defun aero/evil-org-agenda-mode ()
    "Shim in org-agenda evil mode."
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))
  ;; Can't be in :hook because we're defining it here, which is for the byte-compiler
  (add-hook 'org-agenda-mode-hook #'aero/evil-org-agenda-mode))

;; Custom display of org priorities
(package! org-fancy-priorities "harrybournis/org-fancy-priorities"
  :after (org)
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("❗" "⬆" "■" "↓")))

(package! org-ql "alphapapa/org-ql" :after org)


;; Functions for agenda and stuff

(defun archive-all-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree-default)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun aero/insert-todos-from-ritual-with-date ()
  (interactive)
  (let ((ritual-file-path (expand-file-name "template/ritual.org" aero/thornlog-path))
        (today (format-time-string "<%Y-%m-%d>"))
        insertion-point)
    (save-excursion
      ;; Search for daily ritual heading in the current buffer, not the temp buffer
      (goto-char (point-min))
      (when (re-search-forward "^\\*+ Daily Ritual" nil t)
        (setq insertion-point (save-excursion
                                ;; Find the next heading or end of buffer to determine where to insert tasks
                                (if (re-search-forward "^\\*+ " nil t)
                                    (match-beginning 0)
                                  (point-max)))))
      ;; Insert tasks after insertion-point determined from the current buffer
      (when insertion-point
        (with-temp-buffer
          (insert-file-contents ritual-file-path)
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ TODO" nil t)
            (let ((todo-start (match-beginning 0))
                  (todo-end (save-excursion
                              (if (re-search-forward "^\\*+ " nil t)
                                  (match-beginning 0)
                                (point-max)))))
              (let ((todo-entry (buffer-substring-no-properties todo-start todo-end)))
                (with-current-buffer (other-buffer (current-buffer) t)
                  (goto-char insertion-point)
                  (unless (bolp) (insert "\n"))
                  (insert "*" todo-entry "SCHEDULED: " today "\n\n")
                  (setq insertion-point (point)))))))))))

(defun aero/org-agenda-list ()
  "`org-agenda', skipping command menu to list."
  (interactive)
  (org-agenda nil "a"))

(defun aero/org-agenda-todo ()
  "`org-agenda', skipping command menu to todos."
  (interactive)
  (org-agenda nil "t"))

(defun aero/org-agenda-done ()
  "Mark the current TODO as done."
  (interactive)
  (org-agenda-todo "DONE"))

(defun aero/org-agenda-done-and-followup ()
  "Mark the current TODO as done and add another task after it."
  (interactive)
  (org-agenda-todo "DONE")
  (org-agenda-switch-to)
  (org-capture 0 "t"))

(defun aero/org-agenda-new ()
  "Create a new task at the current agenda item."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p 'any)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (call-interactively 'org-agenda-list))))

(defun org-agenda-list-closed-on-last-workday ()
  (interactive)
  (let* ((org-agenda-files (list (buffer-file-name)
                                 (expand-file-name "archive/archive.org" aero/thornlog-path)))
         (today (current-time))
         (weekday (format-time-string "%u" today))
         (days-back (if (string= weekday "1") 3 1)) ; If today is Monday (1), go back 3 days to Friday
         (specific-day (format-time-string "%Y-%m-%d" (time-subtract today (days-to-time days-back)))))
    (org-agenda-list nil specific-day 'day)))

(defun org-agenda-list-last-closed ()
  (interactive)
  (require 'org-ql)
  (require 'dash)
  (let* ((archive-file (expand-file-name "archive/archive.org" aero/thornlog-path))
         (all-files (append org-agenda-files (list archive-file)))
         (last-closed-time
          (find-latest-time
           (-non-nil
            (-mapcat (lambda (file)
                       (org-ql-select file
                         `(closed)
                         :action (lambda ()
                                   (let ((closed-time (org-entry-get (point) "CLOSED")))
                                     (when closed-time
                                       (org-time-string-to-time closed-time))))))
                     all-files)))))
    ;; Proceed only if there is at least one CLOSED entry
    (if last-closed-time
        (let ((date-string (format-time-string "%Y-%m-%d" last-closed-time)))
          (org-agenda-list nil date-string 'day))
      (message "No closed tasks found."))))

(defun insert-todays-timestamp-at-entry-end ()
  "Insert today's timestamp at the end of the current org entry."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (end-of-line)
    (insert " ")
    (org-insert-time-stamp (current-time) nil)))


(provide 'aero-org)

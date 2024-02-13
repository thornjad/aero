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
(require 'outline)
(require 'org-ql)
(require 'dash)

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

  (org-agenda-span 10) ; days to show at a time
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
  (org-agenda-start-day "-7d")
  (org-agenda-inhibit-startup t) ; skip unnecessary loading time
  (org-priority-faces '((?A . error) (?B . warning) (?C . success) (?D . org-priority)))
  (org-archive-location (concat (expand-file-name "archive/archive.org" aero/thornlog-path) "::* From %s"))

  :config
  (aero-leader-def
    "oT" '(:ignore t :wk "time")
    "oTt" 'org-time-stamp
    "oTd" 'insert-todays-timestamp-at-entry-end
    "ot" 'org-todo
    "og" 'org-set-tags-command
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


;; Thornlog management

(defun new-day ()
  "Create a new entry for today, if not already present."
  (interactive)
  (cond
   ((not (string-match "thornlog/log\\.org" (buffer-file-name)))
    (message "Not in Thornlog file"))
   ((today) (message "Entry for today already present"))
   (t (progn
        (new-day-insert)
        (evil-scroll-line-to-center (line-number-at-pos))))))

(defun today ()
  (interactive)
  (let* ((today (format-time-string "<%Y-%m-%d %a>"))
         (entry-found nil)
         (query `(and (level 1)
                      (ts-active :on ,today))))
    (org-ql-select (current-buffer)
      query
      :action (lambda ()
                (setq entry-found t)
                (evil-scroll-line-to-center (point))))
    entry-found))

(defun replace-thornlog-placeholders (template prev-day-date prev-day-summary prev-day-goals prev-day-notes blocked-message)
  "Replace placeholders in TEMPLATE with values from previous sections and BLOCKED-MESSAGE."
  (let* ((day-of-week (calendar-day-name (calendar-current-date)))
         (today (format-time-string "%A, %B %d <%Y-%m-%d>"))
         (yesterday (format-time-string "%A, %B %d <%Y-%m-%d>" (time-subtract (current-time) (days-to-time 1))))
         (since-string (if (string= (match-string 1 prev-day-date) yesterday)
                           "yesterday"
                         (car (split-string prev-day-date ", "))))
         (template (replace-regexp-in-string "<new-day-template>" today template))
         (template (replace-regexp-in-string "<previous-entry-day>" since-string template))
         (template (replace-regexp-in-string "<previous-day-summary>" prev-day-summary template))
         (template (replace-regexp-in-string "<previous-day-goals>" prev-day-goals template))
         (template (replace-regexp-in-string "<previous-day-notes>" prev-day-notes template))
         (template (replace-regexp-in-string "<blocked-message>" blocked-message template)))
    template))

(defun extract-section-content (title)
  "Extract the content of the section with TITLE."
  (save-excursion
    (re-search-forward (regexp-quote title) nil t)
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (let ((content-start (point))
          (content-end (progn (org-end-of-subtree) (point))))
      (string-trim (buffer-substring-no-properties content-start content-end)))))

(defun new-day-insert ()
  "Insert a new day entry based on a template."
  (interactive)
  (setf (point) (point-max))
  (re-search-backward "^\\* [[:alpha:]]+, [[:alpha:]]+ [[:digit:]]+" nil t)
  (let* ((template (with-temp-buffer
                     (insert-file-contents
                      (expand-file-name "template/new-day.org" aero/thornlog-path))
                     (buffer-string)))
         (element (org-element-at-point))
         (prev-day (org-element-property :title element))
         (prev-summary (extract-section-content "*** Today"))
         (prev-goals (extract-section-content "** Goals"))
         (prev-notes (extract-section-content "** Notes"))
         (blocked-message (rand-nth aero/thornlog-blocked-response-list))
         (new-day-entry (replace-thornlog-placeholders
                         template prev-day prev-summary
                         prev-goals prev-notes blocked-message)))
    (outline-hide-sublevels 1)
    (setf (point) (point-max))
    (org-cycle-hide-drawers 'all)
    (org-previous-visible-heading 1)
    (org-show-subtree)
    (org-end-of-subtree)
    (insert "\n\n" new-day-entry "\n")
    (org-previous-visible-heading 1)
    (org-show-subtree)
    (goto-char (point-max))
    (search-backward "*** Today")
    (forward-line)))


(provide 'aero-org)

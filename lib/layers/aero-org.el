;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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
(require 'dash)
(require 'notifications)

(package! org :builtin
  :preface
  (defun archive-buffer-closed-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (when (member (org-get-todo-state) org-done-keywords)
         (org-archive-subtree-default)
         ;; the archive alters the tree, so just go back to the top
         (setq org-map-continue-from (point-min))))
     nil 'file))

  (defun aero/org-collapse-entry-if-done ()
    "Collapse the current entry if it is marked as DONE."
    (when (member org-state '("DONE"))
      (hide-subtree)))

  (defun aero/org-expand-entry-if-todo ()
    "Expand the current entry if it is marked as TODO."
    (when (member org-state '("TODO"))
      (show-subtree)))

  (defun jump-to-org-agenda ()
    "Go to the org agenda. Used on idle timer."
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

  ;; https://ag91.github.io/blog/2022/03/12/org-agenda-keep-the-buffer-order-untouched-after-saving-all-modified-org-buffers/
  (defun aero/reorder-buffer-list (new-list)
    (while new-list
      (bury-buffer (car new-list))
      (setq new-list (cdr new-list))))

  (defun aero/keep-buffer-list-unaltered (orig-fun &rest args)
    (let ((buffers (buffer-list))
          (result (apply orig-fun args)))
      (aero/reorder-buffer-list buffers)
      result))

  (defun org-schedule-and-refile ()
    "Schedule the current entry and refile it."
    (interactive)
    (org-schedule t)
    (org-refile))

  (defun org-deadline-and-refile ()
    "Deadline the current entry and refile it."
    (interactive)
    (org-deadline t)
    (org-refile))

  :custom
  (org-insert-heading-respect-content t) ; insert headings after current subtree
  (org-fold-catch-invisible-edits 'smart) ; don't accidentally remove hidden text
  (org-startup-with-inline-images t) ; default to showing images on startup
  (org-startup-indented t) ; default to indenting properly
  (org-log-done 'time) ; log time when item is marked done
  (org-log-into-drawer t) ; put logs in LOGBOOK
  (org-refile-use-outline-path t) ; show path to outline level during refile
  (org-fontify-done-headline t) ; let theme strike out done items

  ;; always put blank before new headings, but be smart about list items
  (org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

  ;; re-scale images to 400px if no with attribute is set (see
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
  (org-image-actual-width '(400))

  (org-capture-templates
   `(("t" "Inbox Task" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/thornlog-path)
       "Inbox")
      "* TODO [#C] %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nCreated at: %a\n"
      :empty-lines 2)
     ("T" "Deadline/Scheduled Task" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/thornlog-path)
       "Tasks")
      "* TODO [#C] %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\nCreated at: %a\n"
      :empty-lines 2)
     ("p" "Ticket (PR)" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/thornlog-path)
       "Tasks")
      "* TICKET [#C] %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\nCreated at: %a\n"
      :empty-lines 2)
     ("r" "Review (PR or tech design)" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/thornlog-path)
       "Tasks")
      "* TODO [#B] %? :review:\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\nCreated at: %a\n"
      :empty-lines 2)
     ("n" "Note" entry
      (file+headline
       ,(expand-file-name "notes.org" aero/thornlog-path)
       "Notes")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :jump-to-captured t
      :empty-lines 2)
     ("e" "Experimentation idea" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/thornlog-path)
       "Experimentation")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nCreated at: %a\n"
      :empty-lines 2)
     ("R" "Reading" entry
      (file+headline
       ,(expand-file-name "todo.org" aero/thornlog-path)
       "Reading")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\nCreated at: %a\n"
      :empty-lines 2)
     ("s" "Time sink" entry
      (file+headline
       ,(expand-file-name "notes.org" aero/thornlog-path)
       "Time sinks")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 2)
     ("m" "Mistake" entry
      (file+headline
       ,(expand-file-name "notes.org" aero/thornlog-path)
       "Mistakes")
      "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
      :empty-lines 2)))

  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w!)" "BLOCKED(b!)" "|" "DONE(d!)" "REMOVED(k)")
     (sequence "TICKET(T)" "PR(p!)" "|" "DONE(d!)" "CLOSED(x)")))

  (org-use-fast-todo-selection 'expert) ; don't fuck up the window layout
  (org-default-notes-file (expand-file-name "notes.org" aero/thornlog-path))
  (org-priority-faces '((?A . error) (?B . warning) (?C . success) (?D . org-priority)))
  (org-priority-highest ?A)
  (org-priority-lowest ?D) ; default is C
  (org-reverse-note-order nil) ; put notes at the end of the entry, instead of the top
  (org-archive-location (concat aero/thornlog-archive-file "::* From %s"))

  ;; don't consider empty lines between entries to be part of the entry
  (org-cycle-separator-lines -2)

  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))

  ;; Agenda
  (org-agenda-span 3) ; days to show at a time
  (org-agenda-start-day nil) ; day to start at
  (org-agenda-start-on-weekday nil) ; start week on current day

  ;; all agenda files
  (org-agenda-files `(,(expand-file-name "todo.org" aero/thornlog-path)
                      ,(expand-file-name "log.org" aero/thornlog-path)
                      ,(expand-file-name "ritual.org" aero/thornlog-path)
                      ,(expand-file-name "notes.org" aero/thornlog-path)))

  (org-agenda-log-mode-items nil) ; don't show closed nor clocked items
  (org-agenda-tags-column -70) ; shift tags over
  (org-agenda-sticky nil) ; don't bury on close buffer
  (org-agenda-use-tag-inheritance t)
  (org-agenda-show-log t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t) ; don't duplicate deadline & scheduled
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-agenda-window-setup 'current-window) ; stop agenda opening a new window
  (org-agenda-skip-unavailable-files t)
  (org-agenda-show-future-repeats nil) ; don't show repeating tasks on future agenda dates

  :init
  (aero-leader-def
    "oa" '(aero/org-agenda-list :wk "agenda")
    "oA" '(org-agenda :wk "agenda menu")
    "os" 'org-schedule
    "od" 'org-deadline
    "oj" 'org-clock-goto
    "ot" 'org-set-tags-command
    "of" 'aero/org-add-file-tag
    "ol" 'org-store-link
    "oT" '(org-tags-view :wk "list tags")
    "vo" 'org-capture)

  :config
  (aero-mode-leader-def
    :keymaps 'org-mode-map
    "t" 'org-todo
    "d" 'thornlog-new-day
    "f" 'org-forward-heading-same-level
    "F" 'org-backward-heading-same-level
    "w" 'org-open-at-point
    "p" 'org-priority
    "r" 'org-refile
    "s" 'org-set-property
    "i" '(:ignore t :wk "insert")
    "il" '(org-insert-link :wk "link")
    "it" 'org-time-stamp
    "ii" 'org-insert-structure-template
    "id" '(org-insert-drawer :wk "drawer")
    "im" 'insert-meeting-task
    "A" 'archive-buffer-closed-tasks
    "c" '(:ignore t :wk "clock / cell")
    "cc" '(org-babel-execute-src-block :wk "exec cell")
    "ci" 'org-clock-in
    "co" 'org-clock-out
    "ck" 'org-clock-cancel
    "cs" 'org-clock-display
    "ce" 'org-set-effort
    "cE" 'org-clock-modify-effort-estimate)

  ;; keep org-save-all from messing up buffer list
  (advice-add 'org-save-all-org-buffers :around #'aero/keep-buffer-list-unaltered)

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  ;; Collapse entries when they are marked as done, and expand when reopened
  (add-hook 'org-after-todo-state-change-hook #'aero/org-collapse-entry-if-done)
  (add-hook 'org-after-todo-state-change-hook #'aero/org-expand-entry-if-todo)

  ;; Also save after state change
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers)

  ;; start with all levels collapsed
  (add-hook 'org-mode-hook #'org-hide-block-all)

  ;; Save org files when using clock
  (add-hook 'org-clock-in-hook #'org-save-all-org-buffers)
  (add-hook 'org-clock-out-hook #'org-save-all-org-buffers)

  ;; Force org-capture to not open new windows
  (defun aero/org-capture-place-template-dont-delete-windows (oldfun &rest args)
    (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
      (apply oldfun args)))
  (with-eval-after-load "org-capture"
    (advice-add 'org-capture-place-template
                :around #'aero/org-capture-place-template-dont-delete-windows))

  ;; set up stuff for clock persistence
  (org-clock-persistence-insinuate)

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
  :after (evil org org-super-agenda)
  :preface
  (defun aero/evil-org-agenda-mode ()
    "Shim in org-agenda evil mode."
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . aero/evil-org-agenda-mode)))

;; Custom display of org priorities
(package! org-fancy-priorities "harrybournis/org-fancy-priorities"
  :after (org)
  :hook (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list '("❗" "⬆" "■" "↓")))

(package! org-super-agenda "alphapapa/org-super-agenda"
  :preface
  (defun aero/org-super-agenda-without-keymap ()
    "Stops super-agenda from overriding evil-org bindings."
    (org-super-agenda-mode)
    (setq org-super-agenda-header-map (make-sparse-keymap)))

  :hook ((org-agenda-after-show . recenter)
         (org-agenda-mode . aero/org-super-agenda-without-keymap))

  :custom
  (org-super-agenda-groups
   '((:name "Daily Ritual" :tag "ritual")
     (:time-grid t)
     (:name "Priority A" :and (:priority "A" :not (:todo "PR")))
     (:name "Reviews" :tag "review")
     (:name "Tickets and PRs" :todo "PR" :todo "TICKET")
     (:deadline past)
     (:deadline today)
     (:name "Priority B" :and (:priority "B" :not (:todo "WAITING" :todo "BLOCKED")))
     (:name "Past scheduled" :and (:scheduled past :not (:todo "WAITING" :todo "BLOCKED")))
     (:name "Prioritized" :and (:priority<= "B" :not (:todo "WAITING" :todo "BLOCKED")))
     (:name "Waiting/Blocked" :todo "WAITING" :todo "BLOCKED")
     (:name "Due soon" :deadline future)))

  ;; add space between dates by adding space after the final group
  (org-super-agenda-final-group-separator "\n"))

;; Allow drag-and-drop of images from browser, finder, etc.
(package! org-download "abo-abo/org-download"
  :after (org general)
  :custom (org-download-method 'directory)
  :init
  (aero-mode-leader-def
    :keymaps 'org-mode-map
    "ic" '(org-download-clipboard :wk "insert image from clipboard")))


;; Functions for agenda and stuff

(defun aero/org-agenda-list ()
  "`org-agenda', skipping command menu to list."
  (interactive)
  (org-agenda nil "a"))

(defun aero/org-agenda-todo ()
  "`org-agenda', skipping command menu to todos."
  (interactive)
  (org-agenda nil "t"))

(defun aero/org-agenda-new ()
  "Create a new task at the current agenda item."
  (interactive)
  (org-agenda-switch-to)
  (org-capture 0))

(defun org-agenda-list-closed-on-last-workday ()
  (interactive)
  (let* ((org-agenda-files (list (buffer-file-name)
                                 (expand-file-name "archive/archive.org" aero/thornlog-path)
                                 (expand-file-name "log.org" aero/thornlog-path)))
         (today (current-time))
         (weekday (format-time-string "%u" today))
         (days-back (if (string= weekday "1") 3 1)) ; If today is Monday (1), go back 3 days to Friday
         (specific-day (format-time-string "%Y-%m-%d" (time-subtract today (days-to-time days-back))))
         (org-agenda-log-mode-items '(closed))
         (org-agenda-skip-deadline-if-done nil)
         (org-agenda-skip-scheduled-if-done nil)
         (org-agenda-skipp-timestamp-if-done nil)
         (org-super-agenda-groups '((:time-grid t))))
    (org-agenda-list nil specific-day 'day)))

(defun insert-todays-timestamp-at-entry-end ()
  "Insert today's timestamp at the end of the current org entry."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (end-of-line)
    (insert " ")
    (org-insert-time-stamp (current-time) nil)))


;; Thornlog management

(defvar aero/thornlog-blocked-response-list
  '("none" "none" "none" "none" "none" "none" "none"
    "nothing" "nope" "nil" "zilch" "naught" "void" "n/a"
    "∅" "nada" "pas une chose" "無" "żadnych")
  "List of template responses for blocked, to be chosen randomly.

'none' is included multiple times so as to give it increased weight, it being the 'normal'
response. I'm too lazy to create a weights map or something, this is easier.")

(defun thornlog-new-day ()
  "Create a new entry for today, if not already present."
  (interactive)
  (cond
   ((not (string-match "thornlog/log\\.org" (buffer-file-name)))
    (message "Not in Thornlog file"))
   ((thornlog-today) (message "Entry for today already present"))
   (t (progn
        (thornlog-new-day-insert)
        (recenter)))))

(defun thornlog-today ()
  "Jump to today's entry, if present, return t if found."
  (interactive)
  (let ((today-str (format-time-string "* %A, %B %d"))
        (found nil))
    (goto-char (point-max))
    (when  (search-backward today-str nil t)
      (setq found t)
      (outline-show-entry)
      (recenter))
    found))

(defun replace-thornlog-placeholders (template prev-day-date)
  "Replace placeholders in TEMPLATE with reference to PREV-DAY-DATE."
  (let* ((day-of-week (calendar-day-name (calendar-current-date)))
         (today (format-time-string "%A, %B %d"))
         (yesterday (format-time-string "%A, %B %d" (time-subtract (current-time) (days-to-time 1))))
         (since-string (if (string= prev-day-date yesterday)
                           "yesterday"
                         (car (split-string prev-day-date ", "))))
         (template (replace-regexp-in-string "<new-day-template>" today template))
         (template (replace-regexp-in-string "<previous-entry-day>" since-string template))
         (blocked-message (rand-nth aero/thornlog-blocked-response-list))
         (template (replace-regexp-in-string "<blocked-message>" blocked-message template)))
    template))

(defun extract-section-content (title)
  "Extract the content of the section with TITLE."
  (save-excursion
    (re-search-forward (regexp-quote title) nil t)
    (org-back-to-heading t)
    (org-mark-subtree)
    (forward-line 1) ; deselect the heading
    (let ((content
           (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
      (deactivate-mark)
      content)))

(defun thornlog-new-day-insert ()
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
         (new-day-entry (replace-thornlog-placeholders template prev-day)))
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
    (search-backward "*** Since")
    (forward-line)))

(defun aero/thornlog-dir ()
  "Personal persistent log."
  (interactive)
  (declare-function deer "ranger.el")
  (when (require 'ranger nil t)
    (deer aero/thornlog-path)))

(defun aero/thornlog-log ()
  "Personal persistent log."
  (interactive)
  (find-file (expand-file-name "log.org" aero/thornlog-path)))

(defun aero/thornlog-today ()
  "Go to today's log if it exists."
  (interactive)
  (aero/thornlog-log)
  (thornlog-today))

(defun aero/thornlog-notes ()
  "Personal notes file."
  (interactive)
  (find-file (expand-file-name "notes.org" aero/thornlog-path)))

(defun aero/thornlog-todo ()
  "Personal todo list."
  (interactive)
  (find-file (expand-file-name "todo.org" aero/thornlog-path)))

(defun insert-meeting-task ()
  (interactive)
  (let* ((meeting-name (read-string "Meeting Name: "))
         (meeting-time (read-string "Meeting Time (optional): "))
         (today (format-time-string "%Y-%m-%d"))
         (scheduled-string (if (not (string= meeting-time ""))
                               (format "<%s %s>" today meeting-time)
                             (format "<%s>" today)))
         (task-string (format "*** MEETING %s  :meeting:\nSCHEDULED: %s"
                              meeting-name scheduled-string)))
    (goto-char (point-max))
    (re-search-backward "^\\*+ Meetings" nil t)
    (org-end-of-subtree)
    (insert "\n\n" task-string)))

(defun aero/org-add-file-tag ()
  "Prompts for a tag with completion from all org-roam tags and adds it to the file's tags, placing it after the #+title: line if it exists."
  (interactive)
  (let* ((case-fold-search t)
         (all-tags-query "SELECT DISTINCT tag FROM tags")
         (all-tags-result (org-roam-db-query all-tags-query))
         (all-tags (mapcar #'car all-tags-result))
         (tag (completing-read "Tag: " all-tags)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^#\\+filetags: \\(.*\\)$" nil t)
          (let ((existing-tags (match-string-no-properties 1)))
            (beginning-of-line)
            (delete-region (point) (line-end-position))
            (insert (format "#+filetags: %s%s:" existing-tags tag)))
        ;; No existing tags, search for title line to place new tags after
        (goto-char (point-min))
        (if (re-search-forward "^#\\+title:.*$" nil t)
            (progn
              (end-of-line)
              (insert (format "\n#+filetags: :%s:" tag)))
          (goto-char (point-min))
          (insert (format "#+filetags: :%s:\n" tag)))))))


;; Org-roam created and updated timestamps

(defun aero/org-roam-insert-created-property ()
  "Insert a :created: property for a new Org-roam node if it doesn't already have one."
  (interactive)
  (when (org-roam-file-p)
    (unless (org-entry-get (point-min) "created" t)
      (let ((creation-time (aero/org-roam-extract-timestamp-from-filepath
                            (buffer-file-name))))
        (when creation-time
          (save-excursion
            (goto-char (point-min))
            (org-set-property "created" creation-time)))))))

(defun aero/org-roam-extract-timestamp-from-filepath (filepath)
  "Extract timestamp from the Org-roam FILEPATH assuming it follows the default naming scheme."
  (let ((filename (file-name-nondirectory filepath)))
    (when (string-match "\\([0-9]\\{8\\}\\)\\([0-9]\\{4\\}\\)" filename)
      (let ((year (substring filename (match-beginning 1) (+ (match-beginning 1) 4)))
            (month (substring filename (+ (match-beginning 1) 4) (+ (match-beginning 1) 6)))
            (day (substring filename (+ (match-beginning 1) 6) (+ (match-beginning 1) 8)))
            (hour (substring filename (match-beginning 2) (+ (match-beginning 2) 2)))
            (minute (substring filename (+ (match-beginning 2) 2) (+ (match-beginning 2) 4))))
        (let ((time-struct (date-to-time (format "%s-%s-%sT%s:%s" year month day hour minute))))
          (format-time-string "[%Y-%m-%d %a %H:%M]" time-struct))))))

(defun aero/org-roam-insert-modified-property ()
  "Update the :modified: property for an Org-roam node upon saving."
  (when (org-roam-file-p)
    (save-excursion
      (goto-char (point-min))  ; Ensure property is applied to the whole file
      (org-set-property "modified" (format-time-string "[%Y-%m-%d %a %H:%M]")))))

(add-hook 'before-save-hook #'aero/org-roam-insert-created-property)
(add-hook 'before-save-hook #'aero/org-roam-insert-modified-property)


;; Notifications

(defun aero/thornlog-notification (title message)
  "Send a notification with TITLE and MESSAGE."
  (if (featurep 'dbusbind)
      (notifications-notify
       :title title
       :body message
       :app-name "Emacs :: Thornlog")
    (message (format "Thornlog :: %s :: %s" title message))))

(defun aero/thornlog-check-effort-against-clock ()
  "Check if current clock exceeds effort estimate, notify if it has exceeded."
  (when (org-clocking-p)
    (when-let* ((marker (org-entry-get org-clock-marker "Effort"))
                (effort (org-duration-to-minutes marker))
                (clocked (org-clock-get-clocked-time)))
      (when (> clocked effort)
        (aero/thornlog-notification
         "Effort exceeded"
         "The current org task has exceeded its effort estimate.")))))

(defun aero/thornlog-notify-on-excessive-work-time ()
  "Notify when the current org-clock has exceeded the continuous work limit."
  (when-let ((clocked-time (floor (org-time-convert-to-integer (time-since org-clock-start-time))
		                              60)))
    (when (and (org-clocking-p)
               (> clocked-time 120))
      (aero/thornlog-notification
       "Two-hour check-in"
       "You've been working for two hours straight."))))

(defvar aero/thornlog-effort-timer nil
  "Timer for checking effort against clock.")

(defvar aero/thornlog-continuous-work-timer nil
  "Timer for checking continuous work time.")

(defun aero/thornlog-set-effort-timer ()
  "Check if current clock exceeds effort estimate, notify if it has exceeded."
  (setq aero/thornlog-effort-timer (run-with-timer 0 60 'aero/thornlog-check-effort-against-clock)))

(defun aero/thornlog-cancel-effort-timer ()
  "Cancel the effort timer."
  (when aero/thornlog-effort-timer (cancel-timer aero/thornlog-effort-timer)))

(defun aero/thornlog-set-continuous-work-timer ()
  "Notify when org-clock has exceeded the continuous work limit."
  (setq aero/thornlog-continuous-work-timer
        (run-with-timer 0 60 'aero/thornlog-notify-on-excessive-work-time)))

(defun aero/thornlog-cancel-continuous-work-timer ()
  "Cancel the continuous work timer."
  (when aero/thornlog-continuous-work-timer (cancel-timer aero/thornlog-continuous-work-timer)))

(add-hook 'org-clock-in-hook #'aero/thornlog-set-effort-timer)
(add-hook 'org-clock-out-hook #'aero/thornlog-cancel-effort-timer)
(add-hook 'org-clock-cancel-hook #'aero/thornlog-cancel-effort-timer)

(add-hook 'org-clock-in-hook #'aero/thornlog-set-continuous-work-timer)
(add-hook 'org-clock-out-hook #'aero/thornlog-cancel-continuous-work-timer)
(add-hook 'org-clock-cancel-hook #'aero/thornlog-cancel-continuous-work-timer)


;; Roam

(package! org-roam
  (:repo "org-roam/org-roam" :files (:defaults "extensions/*"))
  :defer 1  ; don't load immediately, but soon after init

  :after (general org)

  :custom
  (org-roam-directory (expand-file-name "roam" aero/thornlog-path))
  (org-roam-mode-sections
   (list #'org-roam-backlinks-section
         #'org-roam-reflinks-section
         #'org-roam-unlinked-references-section))

  (org-id-locations-file (expand-file-name ".org-id-locations" aero-cache-dir))

  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :immediate-finish t  ; skip capture buffer, just open the file
      :unnarrowed t)))

  (org-roam-node-display-template
   (concat "${title} " (propertize "${tags}" 'face 'org-tag)))

  :config
  (org-roam-db-autosync-mode)

  (aero-leader-def
    "vf" 'org-roam-node-find
    "vF" 'org-roam-capture
    "vi" 'org-roam-node-insert
    "vb" 'org-roam-buffer-toggle))


(provide 'aero-org)

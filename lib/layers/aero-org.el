;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019, 2021 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package org :straight nil
	:commands org-mode
	:mode ("\\.org\\'" . org-mode)

	:config
	(setq org-src-preserve-indentation t
				org-footnote-auto-adjust t
				org-footnote-section nil
				org-startup-with-inline-images t
				org-startup-indented t)

	;; rescale images to 400px if no with attribute is set (see
	;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
	(setq org-image-actual-width '(400))

  ;; org tries to take this binding back, so wrest control back once more
  (define-key org-mode-map (kbd "M-h") #'windmove-left)

  ;; start with all levels collapsed
  (add-hook 'org-mode-hook #'org-hide-block-all)

  (defun aero/org-element-descendant-of (type element)
    "Return non-nil if ELEMENT is a descendant of TYPE.
TYPE should be an element type, like `item' or `paragraph'.
ELEMENT should be a list like that returned by `org-element-context'."
    ;; MAYBE: Use `org-element-lineage'.
    (when-let* ((parent (org-element-property :parent element)))
      (or (eq type (car parent))
          (aero/org-element-descendant-of type parent))))

  (defun aero/org-return-dwim (&optional default)
    "A helpful replacement for `org-return'.  With prefix, call `org-return'.

On headings, move point to position after entry content.  In
lists, insert a new item or end the list, with checkbox if
appropriate.  In tables, insert a new row or end the table."
    ;; Inspired by John Kitchin: http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
    (interactive "P")
    (if default
        (org-return)
      (cond
       ((org-at-heading-p)
        ;; Heading: insert heading after
        (let ((current-prefix-arg '(4)))
          (aero/voidvar! current-prefix-arg)
          (call-interactively #'org-insert-heading)))

       ((org-at-item-checkbox-p)
        ;; Checkbox: Insert new item with checkbox.
        (org-insert-todo-heading nil))

       ((org-in-item-p)
        ;; Plain list.  Yes, this gets a little complicated...
        (let ((context (org-element-context)))
          (if (or (eq 'plain-list (car context))  ; First item in list
                  (and (eq 'item (car context))
                       (not (eq (org-element-property :contents-begin context)
                                (org-element-property :contents-end context))))
                  (aero/org-element-descendant-of 'item context))  ; Element in list item, e.g. a link
              ;; Non-empty item: Add new item.
              (org-insert-item)
            ;; Empty item: Close the list.
            ;; TODO: Do this with org functions rather than operating on the
            ;; text. Can't seem to find the right function.
            (delete-region (line-beginning-position) (line-end-position))
            (insert "\n"))))

       ((when (fboundp 'org-inlinetask-in-task-p)
          (org-inlinetask-in-task-p))
        ;; Inline task: Don't insert a new heading.
        (org-return))

       ((org-at-table-p)
        (cond ((save-excursion
                 (beginning-of-line)
                 ;; See `org-table-next-field'.
                 (declare-function org-element-table-cell-parser "org-element.el")
                 (declare-function org-element-property "org-element.el")
                 (cl-loop with end = (line-end-position)
                          for cell = (org-element-table-cell-parser)
                          always (equal (org-element-property :contents-begin cell)
                                        (org-element-property :contents-end cell))
                          while (re-search-forward "|" end t)))
               ;; Empty row: end the table.
               (delete-region (line-beginning-position) (line-end-position))
               (org-return))
              (t
               ;; Non-empty row: call `org-return'.
               (org-return))))
       (t
        ;; All other cases: call `org-return'.
        (org-return)))))

  (use-package toc-org :straight t)

  (use-package org-bullets :straight t
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  (aero-mode-leader-def
    :keymaps 'org-mode-map
    (kbd "RET") 'aero/org-return-dwim
    "i" '(:ignore t :wk "insert")
    "il" '(org-insert-link :wk "link")
    "id" '(org-insert-drawer :wk "drawer")
    "c" '(:ignore t :wk "cell")
    "cc" '(org-babel-execute-src-block :wk "exec cell")
    "t" 'today
    "h" '(outline-hide-body :wk "hide all")
    "s" '(outline-show-all :wk "show all")
    "d" 'new-day)

  (defvar aero/org-eval-safe-list
    '(expand-file-name "~/doc/thornlog/")
    "Directories which will have their contents evaluated without prompting.")

  (defun aero/safe-org-file-p (file)
    "Determine if given filename is in a safe org path.
Safe org paths are determined by `aero/org-eval-safe-list'."
    (let ((file-path (file-name-directory file)))
      (seq-some (lambda (x) (string= x file-path)) aero/org-eval-safe-list)))

  (defun aero/org-eval-startblock ()
    "Evaluate the content of a code-block named 'aero/startblock' in the current
  org-document, if present.

  Emacs would usually prompt for permission as a safety precaution, but if the
  buffer is associated with a filename matching any of the patterns inside the
  list aero/org-eval-safe-list we just allow it. "
    (aero/org-eval-named-block "aero/startblock"))

  (defun aero/org-eval-saveblock ()
    "Evaluate the content of a code-block named 'aero/saveblock' in the current
  org-document, if present.

  Emacs would usually prompt for permission as a safety precaution, but if the
  buffer is associated with a filename matching any of the patterns inside the
  list aero/org-eval-safe-list we just allow it."
    (aero/org-eval-named-block "aero/saveblock"))

  (defun aero/org-eval-named-block (name)
    "Execute the named block, if it exists, from within the current file."
    (save-excursion
      (org-save-outline-visibility t
        (when (and (aero/safe-org-file-p (buffer-file-name))
                   (member name (org-babel-src-block-names)))
          (progn
            (setq-local org-confirm-babel-evaluate nil)
            (org-babel-goto-named-src-block name)
            (org-babel-execute-src-block))))))
  (add-hook 'org-mode-hook #'aero/org-eval-startblock)

  ;; evaluation the save-block on save
  (defun aero/org-mode-before-save-hook-eval ()
    (when (eq major-mode 'org-mode)
      (aero/org-eval-saveblock)))
  (add-hook 'before-save-hook #'aero/org-mode-before-save-hook-eval))

(use-package company-org-block :straight (:host github :repo "xenodium/company-org-block")
  :after (org company)
  :custom (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

(use-package org-toc
  :straight (:host github :repo "snosov1/toc-org")
  :after (org)
  :commands (toc-org-mode)
  :init (add-hook 'org-mode-hook #'toc-org-mode))

;; org-mode seems to never call its own hook??
(add-hook
 'prog-mode-hook
 (lambda ()
   (when (eq major-mode "org-mode")
     (message "okay")
     (run-hooks 'org-mode-hook)
     (message "wtf"))))


;; thornlog (agenda) helpers

(defun new-day ()
  "Create a new entry for today, if one isn't already present."
  (interactive)
  (if (today t)
      (message "Entry for today already present")
    (progn
      (new-day-insert)
      (when (require 'evil nil t)
        (evil-scroll-line-to-center (line-number-at-pos))))))

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
  (re-search-backward "^\\* [[:alpha:]]+, [[:alpha:]]" nil t))

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

(defun last-days-today ()
  "Get the last-day's today field"
  (save-excursion
    (last-day)
    (re-search-forward "Today:" nil t)
    (forward-line 1)
    (beginning-of-line)
    (let ((start (point)))
      (re-search-forward "** Meetings" nil t)
      (beginning-of-line)
      (backward-char 1)
      (let ((end (point)))
        (buffer-substring start end)))))

(defvar aero/thornlog-template
  "* TEMPLATE
** Sync summary
Yesterday:

Today:
** Meetings
*** 0930-0945 - Nucleus standup
*** 1015-1030 - Aviator standup
** TODO :noexport: [0%] [0/3]
*** TODO Prep sync summary
*** TODO Email
*** TODO Check on PRs
** Notes"
  "Template for a new day in the thornlog")

(defun new-day-insert ()
  "Insert the contents of the template into the document, for a new day's work."
  (let ((text nil)
        (case-fold-search nil)) ; This ensures our replacements match "HOURS" not "Worked Hours"
    (setf (point) (point-max))
    (save-excursion
      (setq text aero/thornlog-template)

      ;; Replace all our template-pairs
      (dolist (item new-day-template-variables)
        (setq text (replace-regexp-in-string (car item) (apply (cdr item)) text t)))
      ;; Fill in yesterday's status as a head start
      (when (last-day-was-last-workday-p)
        (setq text (replace-regexp-in-string
                    "\\(Yesterday\\|Friday\\):"
                    (concat "Yesterday:\n" (last-days-today))
                    text t)))
      ;; Skip the weekend on Monday
      (when (string= (day-of-week) "Monday")
        (setq text (replace-regexp-in-string "Yesterday:" "Friday:" text t)))

      ;; Done, insert
      (insert text "\n"))
    (forward-line 1)
    (outline-hide-sublevels 1)
    (outline-show-subtree)))

(defun today (&optional nomsg)
  "Visit today's entry, if it exists, message if NOMSG is nil."
  (interactive)
  (let ((pos nil))
    (save-excursion
      (org-save-outline-visibility t
        (setf (point) (point-min))
        (if (re-search-forward (format-time-string "^\\*.* (%Y-%m-%d)") nil t)
            (setq pos (point))
          (unless nomsg (message "No entry for today found.")))))
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
  (find-file (expand-file-name "log.org" aero/thornlog-path)))

(defun aero/thornlog-todo ()
  "Personal persistent log todo."
  (interactive)
  (find-file (expand-file-name "todo.org" aero/thornlog-path)))

(provide 'aero-org)

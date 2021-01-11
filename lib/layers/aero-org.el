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

(use-package org
	:straight org-plus-contrib
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

  ;; Patch to ensure org-return always inserts an item /after/ the current item.
  ;; Default behavior inserts before the current item depending on pointer
  ;; position.
  (el-patch-feature org-list)
  (el-patch-defun org-list-insert-item (pos struct prevs &optional checkbox after-bullet)
    "Insert a new list item at POS and return the new structure.
STRUCT is the list structure.  PREVS is the alist of previous
items, as returned by `org-list-prevs-alist'.

Insert a checkbox if CHECKBOX is non-nil, and string AFTER-BULLET
after the bullet.  Cursor will be after this text once the
function ends.

This function modifies STRUCT."
    (let* ((case-fold-search t)
	         (item
	          (catch :exit
	            (let ((i nil))
	              (pcase-dolist (`(,start ,_ ,_ ,_ ,_ ,_ ,end) struct)
		              (cond
		               ((> start pos) (throw :exit i))
		               ((< end pos) nil)	;skip sub-lists before point
		               (t (setq i start))))
	              ;; If no suitable item is found, insert a sibling of the
	              ;; last item in buffer.
	              (or i (caar (reverse struct))))))
	         (item-end (org-list-get-item-end item struct))
	         (item-end-no-blank (org-list-get-item-end-before-blank item struct))
	         (beforep
	          (progn
	            (goto-char item)
	            (looking-at org-list-full-item-re)
	            (<= pos
		              (cond
		               ((not (match-beginning 4)) (match-end 0))
		               ;; Ignore tag in a non-descriptive list.
		               ((save-match-data (string-match "[.)]" (match-string 1)))
		                (match-beginning 4))
		               (t (el-patch-swap
                        (save-excursion
		                      (goto-char (match-end 4))
		                      (skip-chars-forward " \t")
		                      (point))
                        (point)))))))
	         (split-line-p (org-get-alist-option org-M-RET-may-split-line 'item))
	         (blank-nb (org-list-separating-blank-lines-number pos struct prevs))
	         ;; Build the new item to be created.  Concatenate same bullet
	         ;; as item, checkbox, text AFTER-BULLET if provided, and text
	         ;; cut from point to end of item (TEXT-CUT) to form item's
	         ;; BODY.  TEXT-CUT depends on BEFOREP and SPLIT-LINE-P.  The
	         ;; difference of size between what was cut and what was
	         ;; inserted in buffer is stored in SIZE-OFFSET.
	         (ind (org-list-get-ind item struct))
	         (ind-size (if indent-tabs-mode
		                     (+ (/ ind tab-width) (mod ind tab-width))
		                   ind))
	         (bullet (org-list-bullet-string (org-list-get-bullet item struct)))
	         (box (and checkbox "[ ]"))
	         (text-cut
	          (and (not beforep)
	               split-line-p
	               (progn
		               (goto-char pos)
		               ;; If POS is greater than ITEM-END, then point is in
		               ;; some white lines after the end of the list.  Those
		               ;; must be removed, or they will be left, stacking up
		               ;; after the list.
		               (when (< item-end pos)
		                 (delete-region (1- item-end) (point-at-eol)))
		               (skip-chars-backward " \r\t\n")
		               ;; Cut position is after any blank on the line.
		               (save-excursion
		                 (skip-chars-forward " \t")
		                 (setq pos (point)))
		               (delete-and-extract-region (point) item-end-no-blank))))
	         (body
	          (concat bullet
		                (and box (concat box " "))
		                after-bullet
		                (and text-cut
		                     (if (string-match "\\`[ \t]+" text-cut)
			                       (replace-match "" t t text-cut)
			                     text-cut))))
	         (item-sep (make-string  (1+ blank-nb) ?\n))
	         (item-size (+ ind-size (length body) (length item-sep)))
	         (size-offset (- item-size (length text-cut))))
      ;; Insert effectively item into buffer.
      (goto-char item)
      (indent-to-column ind)
      (insert body item-sep)
      ;; Add new item to STRUCT.
      (dolist (e struct)
        (let ((p (car e)) (end (nth 6 e)))
	        (cond
	         ;; Before inserted item, positions don't change but an item
	         ;; ending after insertion has its end shifted by SIZE-OFFSET.
	         ((< p item)
	          (when (> end item)
	            (setcar (nthcdr 6 e) (+ end size-offset))))
	         ;; Item where insertion happens may be split in two parts.
	         ;; In this case, move start by ITEM-SIZE and end by
	         ;; SIZE-OFFSET.
	         ((and (= p item) (not beforep) split-line-p)
	          (setcar e (+ p item-size))
	          (setcar (nthcdr 6 e) (+ end size-offset)))
	         ;; Items starting after modified item fall into two
	         ;; categories.
	         ;;
	         ;; If modified item was split, and current sub-item was
	         ;; located after split point, it was moved to the new item:
	         ;; the part between body start and split point (POS) was
	         ;; removed.  So we compute the length of that part and shift
	         ;; item's positions accordingly.
	         ;;
	         ;; Otherwise, the item was simply shifted by SIZE-OFFSET.
	         ((and split-line-p (not beforep) (>= p pos) (<= p item-end-no-blank))
	          (let ((offset (- pos item ind (length bullet) (length after-bullet))))
	            (setcar e (- p offset))
	            (setcar (nthcdr 6 e) (- end offset))))
	         (t
	          (setcar e (+ p size-offset))
	          (setcar (nthcdr 6 e) (+ end size-offset))))))
      (push (list item ind bullet nil box nil (+ item item-size)) struct)
      (setq struct (sort struct #'car-less-than-car))
      ;; If not BEFOREP, new item must appear after ITEM, so exchange
      ;; ITEM with the next item in list.  Position cursor after bullet,
      ;; counter, checkbox, and label.
      (if beforep
	        (goto-char item)
        (setq struct (org-list-swap-items item (+ item item-size) struct))
        (goto-char (org-list-get-next-item
		                item struct (org-list-prevs-alist struct))))
      struct))

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
       ;; Act depending on context around point.

       ((org-at-heading-p)
        ;; Heading: Move to position after entry content.
        ;; NOTE: This is probably the most interesting feature of this function.
        (let ((heading-start (org-entry-beginning-position)))
          (goto-char (org-entry-end-position))
          (cond ((and (org-at-heading-p)
                      (= heading-start (org-entry-beginning-position)))
                 ;; Entry ends on its heading; add newline after
                 (end-of-line)
                 (insert "\n\n"))
                (t
                 ;; Entry ends after its heading; back up
                 (forward-line -1)
                 (end-of-line)
                 (when (org-at-heading-p)
                   ;; At the same heading
                   (forward-line)
                   (insert "\n")
                   (forward-line -1))
                 (while (not (looking-back (rx (repeat 3 (seq (optional blank) "\n"))) nil))
                   (insert "\n"))
                 (forward-line -1)))))

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

  (use-package org-superstar :straight t
    :config
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

  (aero-mode-leader-def
    :keymaps 'org-mode-map
    (kbd "RET") 'aero/org-return-dwim
    "i" '(:ignore t :wk "insert")
    "il" '(org-insert-link :wk "link")
    "id" '(org-insert-drawer :wk "drawer")
    "c" '(:ignore t :wk "cell")
    "cc" '(org-babel-execute-src-block :wk "exec cell"))

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
  (if (today)
      (message "Entry for today already present")
    (new-day-insert)))

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

(defun new-day-insert ()
  "Insert the contents of a template into the document, for a new day's work.
This function inserts the block found between '* TEMPLATE' and
'END' then fills in the date nicely."
  (let ((start nil)
        (text nil)
        (case-fold-search nil) ; This ensures our replacements match "HOURS" not "Worked Hours"
        (end nil))
    (save-excursion
      (outline-show-all)
      (goto-line 0)
      (re-search-forward "^\* TEMPLATE" )
      (beginning-of-line)
      (backward-char 1)
      (setq start (point))
      (next-line 2)
      (re-search-forward "END$")
      (beginning-of-line)
      (backward-char 1)
      (setq end (point))
      (setq text (buffer-substring start end))
      (goto-char start)

      ;; Replace all our template-pairs
      (dolist (item new-day-template-variables)
        (setq text (replace-regexp-in-string (car item) (apply (cdr item)) text t)))
      ;; Skip the weekend on Monday
      (when (string-match "^\* Monday " text)
        (setq text (replace-regexp-in-string "Yesterday:" "Friday:" text t)))

      ;; Done, insert
      (insert text "\n"))
    (goto-char start)
    (next-line 1)
    (outline-hide-sublevels 1)))

;; Jump to today's entry.
(defun today ()
  "Visit today's entry, if it exists.  Otherwise show a message."
  (interactive)
  (let ((pos nil))
    (save-excursion
      (org-save-outline-visibility t
        (outline-show-all)
        (goto-line 0)
        (if (re-search-forward (format-time-string "^\\*.* (%Y-%m-%d)") nil t)
            (setq pos (point))
          (message "No entry for today found."))))
    (if pos
        (progn
          (outline-show-all)
          (goto-char pos)
          (outline-hide-sublevels 1)
          t)
      nil)))

(defun clear-subtree ()
  "Delete the subtree we're inside.

    We move to the start of the heading, record our position, then the
    end of the tree and work backwards until we've gone too far."
  (let (start)
    (save-excursion
      (org-back-to-heading t)
      (setq start (point))
      (org-end-of-subtree t)
      (while (>= (point) start)
        (delete-char -1)))))

(defun remove-empty-sections (backend)
  "If there are any headings which contain only 'empty' content
    then don't show them on export

    Empty here means either literally empty, or having the content
    'None' or 'None.'."
  (save-excursion
    (outline-show-all)
    (goto-line 0)

    (org-map-entries
     '(lambda ()
        (if (or (equalp "None." (format "%s" (org-get-entry)))
                (equalp "None" (format "%s" (org-get-entry)))
                (equalp "" (format "%s" (org-get-entry))))
            (clear-subtree))))))

(provide 'aero-org)

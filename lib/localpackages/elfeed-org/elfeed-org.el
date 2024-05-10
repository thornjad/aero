;;; elfeed-org.el --- Configure elfeed with one or more org-mode files -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Jade Michael Thornton
;; Copyright (c) 2014 Remy Honig

;; This program is free software; you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:

;; A system for keeping elfeed RSS feeds and their tags in an org file. This package is based on
;; elfeed-org by Remy Honig, but has been simplified and updated to suit my tastes.

;; As this package is modified from the original elfeed-org, which carried a GPLv3 license, this
;; package is also licensed under the GPLv3. This license does not extend to the rest of Aero Emacs,
;; see the top-level README for more information on Aero Emacs. This version of this package removes
;; the option to use any future versions of the GPL, only version 3. I do not personally like the
;; GPL and do not wish to automatically upgrade to future versions of it.

;; Example:
;; ``` org
;; * Blogs                                                              :elfeed:
;; Text under headlines is ignored
;; ** https://example.com/feed.xml  :feedtag:
;; ** [[http://orgmode.org][Org Mode Links supported as well]]
;; ** Emacs  :emacs:
;; *** https://sachachua.com/blog/category/emacs/feed/
;; Entries inherit tags from their parents
;;
;; ```

;;; Code:

(require 'elfeed)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'xml)

(defgroup elfeed-org nil
  "Configure the Elfeed RSS reader with an Orgmode file"
  :prefix "elfeed-org-"
  :group 'comm)

(defcustom elfeed-org-tree-id "elfeed"
  "The tag or ID property on the trees containing the RSS feeds."
  :group 'elfeed-org
  :type 'string)

(defcustom elfeed-org-auto-ignore-invalid-feeds nil
  "Tag feeds to ignore them when a feed could not loaded."
  :group 'elfeed-org
  :type 'bool)

(defcustom elfeed-org-files (list (locate-user-emacs-file "elfeed.org"))
  "The files where we look to find trees with the `elfeed-org-tree-id'.
In this file paths can be given relative to `org-directory'."
  :group 'elfeed-org
  :type '(repeat (file :tag "org-mode file")))

(defun elfeed-org-check-configuration-file (file)
  "Make sure FILE exists."
  (when (not (file-exists-p (expand-file-name file org-directory)))
    (error "Elfeed-org cannot open %s.  Make sure it exists or customize the variable \'elfeed-org-files\'"
           (abbreviate-file-name file))))

(defun elfeed-org-is-headline-contained-in-elfeed-tree ()
  "Is any ancestor a headline with the elfeed tree id.
Return t if it does or nil if it does not."
  (let ((result nil))
    (save-excursion
      (while (and (not result) (org-up-heading-safe))
        (setq result (member elfeed-org-tree-id (org-get-tags))))
      result)))

(defun elfeed-org-import-trees (tree-id)
  "Get trees with \":ID:\" property or tag of value TREE-ID.
Return trees with TREE-ID as the value of the id property or
with a tag of the same value.  Setting an \":ID:\" property is not
recommended but I support it for backward compatibility of
current users."
  (org-element-map
      (org-element-parse-buffer)
      'headline
    (lambda (h) h)))

(defun elfeed-org-convert-tree-to-headlines (parsed-org)
  "Get the inherited tags from PARSED-ORG structure if MATCH-FUNC is t.
The algorithm to gather inherited tags depends on the tree being
visited depth first by `org-element-map'.  The reason I don't use
`org-get-tags-at' for this is that I can reuse the parsed org
structure and I am not dependent on the setting of
`org-use-tag-inheritance' or an org buffer being present at
all.  Which in my opinion makes the process more traceable."
  (let* ((tags '())
         (level 1))
    (org-element-map parsed-org 'headline
      (lambda (h)
        (pcase-let*
            ((current-level (org-element-property :level h))
             (delta-level (- current-level level))
             (delta-tags (mapcar (lambda (tag)
                                   (intern (substring-no-properties tag)))
                                 (org-element-property :tags h)))
             (heading (org-element-property :raw-value h))
             (`(,link ,description)
              (org-element-map (org-element-property :title h) 'link
                (lambda (link)
                  (list
                   (org-element-property :raw-link link)
                   (when (and (org-element-property :contents-begin link)
                              (org-element-property :contents-end link))
                     (buffer-substring
                      (org-element-property :contents-begin link)
                      (org-element-property :contents-end link)))))
                nil t)))
          ;; update the tags stack when we visit a parent or sibling
          (unless (> delta-level 0)
            (let ((drop-num (+ 1 (- delta-level))))
              (setq tags (nthcdr drop-num tags))))
          ;; save current level to compare with next heading that will be visited
          (setq level current-level)
          ;; save the tags that might apply to potential children of the current heading
          (push (append (car tags) delta-tags) tags)
          ;; return the heading and inherited tags
          (if (and link description)
              (append (list link)
                      (car tags)
                      (list description))
            (append (list (if link link heading))
                    (car tags))))))))

;; TODO: mark wrongly formatted feeds (PoC for unretrievable feeds)
(defun elfeed-org-flag-headlines (parsed-org)
  "Flag headlines in PARSED-ORG if they don't have a valid value."
  (org-element-map parsed-org 'headline
    (lambda (h)
      (let ((tags (org-element-property :tags h)))
        (org-element-put-property h :tags (push "_flag_" tags))))))

(defun elfeed-org-filter-relevant (list)
  "Filter relevant entries from the LIST."
  (cl-remove-if-not
   (lambda (entry)
     (string-match-p "\\(http\\|gopher\\|file\\)" (car entry)))
   list))

(defun elfeed-org-cleanup-headlines (headlines tree-id)
  "In all HEADLINES given remove the TREE-ID."
  (mapcar (lambda (e) (delete tree-id e)) headlines))

(defun elfeed-org-import-headlines-from-files (files tree-id)
  "Visit all FILES and return the headlines stored under tree tagged TREE-ID or with the \":ID:\" TREE-ID in one list."
  (cl-remove-duplicates
   (mapcan (lambda (file)
             (with-temp-buffer
               (insert-file-contents (expand-file-name file org-directory))
               (let ((org-inhibit-startup t)
                     (org-mode-hook nil))
                 (org-mode))
               (elfeed-org-cleanup-headlines
                (elfeed-org-filter-relevant
                 (elfeed-org-convert-tree-to-headlines
                  (elfeed-org-import-trees tree-id)))
                (intern tree-id))))
           files)
   :test #'equal))

(defun elfeed-org-convert-headline-to-tagger-params (tagger-headline)
  "Add new entry hooks for tagging configured with the found headline in TAGGER-HEADLINE."
  (list (string-clean-whitespace (car tagger-headline))
        (cdr tagger-headline)))

(defun elfeed-org-export-feed (headline)
  "Export HEADLINE to the proper `elfeed' structure."
  (if (and (stringp (car (last headline)))
           (> (length headline) 1))
      (progn
        (add-to-list 'elfeed-feeds (butlast headline))
        (let ((feed (elfeed-db-get-feed (car headline)))
              (title (substring-no-properties (car (last headline)))))
          (setf (elfeed-meta feed :title) title)
          (elfeed-meta feed :title)))
    (add-to-list 'elfeed-feeds headline)))

(defun elfeed-org-process (files tree-id)
  "Process headlines from FILES with org headlines with TREE-ID."

  (mapc #'elfeed-org-check-configuration-file files)
  (setq elfeed-feeds nil)

  ;; Convert org structure to elfeed structure and register subscriptions
  (let* ((headlines (elfeed-org-import-headlines-from-files files tree-id))
         (subscriptions (elfeed-org-filter-subscriptions headlines)))
    (mapc #'elfeed-org-export-feed subscriptions))

  ;; Tell user what we did
  (elfeed-log 'info "elfeed-org loaded %i feeds" (length elfeed-feeds)))

(defun elfeed-org-filter-subscriptions (headlines)
  "Filter subscriptions to rss feeds from the HEADLINES in the tree."
  (cl-remove-if-not #'identity
                    (mapcar
                     (lambda (headline)
                       (let* ((text (car headline))
                              (link-and-title (and (string-match "^\\[\\[\\(http.+?\\)\\]\\[\\(.+?\\)\\]\\]" text)
                                                   (list (match-string-no-properties 0 text)
                                                         (match-string-no-properties 1 text)
                                                         (match-string-no-properties 2 text))))
                              (hyperlink (and (string-match "^\\[\\[\\(http.+?\\)\\]\\(?:\\[.+?\\]\\)?\\]" text)
                                              (list (match-string-no-properties 0 text)
                                                    (match-string-no-properties 1 text)))))
                         (cond ((string-prefix-p "http" text) headline)
                               (link-and-title (append (list (nth 1 hyperlink))
                                                       (cdr headline)
                                                       (list (nth 2 link-and-title))))
                               (hyperlink (append (list (nth 1 hyperlink)) (cdr headline))))))
                     headlines)))

(defun elfeed-org-process-advice ()
  "Advice to add to `elfeed' to load the configuration before it is run."
  (elfeed-org-process elfeed-org-files elfeed-org-tree-id))

;;;###autoload
(defun elfeed-org ()
  "Hook up elfeed-org to read the `org-mode' configuration when elfeed is run."
  (interactive)
  (elfeed-log 'info "elfeed-org is set up to handle elfeed configuration")
  (advice-add #'elfeed :before #'elfeed-org-process-advice)
  (add-hook 'elfeed-http-error-hooks
            (lambda (url _status)
              (when elfeed-org-auto-ignore-invalid-feeds
                (elfeed-org-mark-feed-ignore url))))
  (add-hook 'elfeed-parse-error-hooks
            (lambda (url _error)
              (when elfeed-org-auto-ignore-invalid-feeds
                (elfeed-org-mark-feed-ignore url)))))

(provide 'elfeed-org)
;;; elfeed-org.el ends here

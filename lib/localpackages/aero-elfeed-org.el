;;; aero-elfeed-org.el --- Configure elfeed with one or more org-mode files -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Jade Michael Thornton
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

;;; Commentary:

;; A system for keeping elfeed RSS feeds and their tags in an org file. This package is kind of like aero-elfeed-org by Remy Honig, but was written from scratch to significantly simplify the ingestion of entries. unlike Remy's, this package has no export functionality and does not support top-level org links.
;;
;; Text under headlines is ignored
;;
;; Example:
;;
;; * https://example.com/feed.xml  :feedtag:
;; * Emacs  :emacs:
;; ** https://sachachua.com/blog/category/emacs/feed/
;; :PROPERTIES:
;; :feed_title: Testing Title
;; :END:
;;

;;; Code:

(require 'elfeed)
(require 'org)
(require 'org-element)

(defgroup aero-elfeed-org nil
  "Configure the Elfeed RSS reader with an Orgmode file"
  :prefix "aero-elfeed-org-"
  :group 'comm)

(defcustom aero-elfeed-org-file (locate-user-emacs-file "elfeed.org")
  "The org file to find feeds in"
  :group 'aero-elfeed-org
  :type 'file)

(defun aero-elfeed-org-get-links-from-file (file)
  "Extracts links and their tags from the given org FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let (links)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (element)
          (let ((raw-link (org-element-property :raw-link element))
                tags)

            ;; Traverse up the tree to collect tags from parent headlines
            (while (setq element (org-element-property :parent element))
              (when (eq (org-element-type element) 'headline)
                (let ((headline-tags (org-element-property :tags element)))
                  (when headline-tags
                    (setq tags (append tags (mapcar 'intern headline-tags)))))))
            (when-let ((feed-title (org-entry-get element "feed_title")))
              (setf (elfeed-meta (elfeed-db-get-feed raw-link) :title) feed-title))
            (push (if tags (cons raw-link tags) raw-link) links))))
      links)))

;;;###autoload
(defun aero-elfeed-org ()
  "Set `elfeed-feeds' from the org file specified in `aero-elfeed-org-file'."
  (interactive)
  (setq elfeed-feeds (aero-elfeed-org-get-links-from-file aero-elfeed-org-file))
  (elfeed-log 'info "aero-elfeed-org loaded %i feeds" (length elfeed-feeds)))

;;;###autoload
(defun aero-elfeed-org-reload ()
  "Refresh the aero-elfeed-org list."
  (interactive)
  (aero-elfeed-org))

(provide 'aero-elfeed-org)
;;; aero-elfeed-org.el ends here

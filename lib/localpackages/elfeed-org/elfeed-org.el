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

;; Text under headlines is ignored

;; Example:
;;
;; ``` org
;; * https://example.com/feed.xml  :feedtag:
;; * Emacs  :emacs:
;; ** https://sachachua.com/blog/category/emacs/feed/
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

(defcustom elfeed-org-file (locate-user-emacs-file "elfeed.org")
  "The file where we look to find trees with RSS feeds."
  :group 'elfeed-org
  :type 'file)

(defun elfeed-org-get-links-from-file (file)
  "Extracts links and their tags from the given org FILE, maintaining the original link format."
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
            (push (if tags (cons raw-link tags) raw-link) links))))
      links)))

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

;;;###autoload
(defun elfeed-org ()
  "Set `elfeed-feeds' from the org file specified in `elfeed-org-file'."
  (interactive)
  (setq elfeed-feeds (elfeed-org-get-links-from-file elfeed-org-file))
  (elfeed-log 'info "elfeed-org loaded %i feeds" (length elfeed-feeds)))

;;;###autoload
(defun elfeed-org-reload ()
  "Refresh the elfeed-org list."
  (interactive)
  (elfeed-org))

(provide 'elfeed-org)
;;; elfeed-org.el ends here

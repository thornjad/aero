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

(require 'aero-prelude)

(defun aero/ddg (&optional term)
  (interactive "sSearch DuckDuckGo: ")
  (eww-browse-url (format "https://duckduckgo.com/lite?q=%s" (or term ""))))

(defun aero/wiki (&optional term)
  (interactive "sSearch Wikipedia: ")
  (aero/ddg (format "!w %s" (or term ""))))

(defun aero/wiki-news () (interactive)
       (eww-browse-url "https://en.wikipedia.org/wiki/Portal:Current_events"))

(defun aero/npr-news () (interactive)
       (eww-browse-url "https://text.npr.org/"))

(use-package eww :straight nil
  :commands (eww
             eww-browse-url
             eww-search-words)
  :init
  (setq browse-url-browser-function #'eww-browse-url)
	:config
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "ws" '(eww-search-words :which-key "web search")
   "wb" '(:ignore t :wk "browse")
   "wbd" '(aero/ddg :wk "duckduckgo")
   "wbw" '(aero/wiki :wk "wikipedia")
   "wbn" '(:ignore t :wk "news sites")
   "wbnw" '(aero/wiki-news :wk "wikipedia")
   "wbnn" '(aero/npr-news :wk "npr")
   "ww" 'eww
   "wp" 'browse-url-at-point)

	:config
	(setq eww-search-prefix "https://duckduckgo.com/lite?q=")

	(add-hook
   'eww-after-render-hook
   (lambda ()
		 "Rename eww mode buffer so the title of the page is displayed, making
		 fake-tabbed-browsing easier"
		 (let ((title (plist-get eww-data :title)))
			 (when (eq major-mode 'eww-mode)
				 (if title
						 (rename-buffer (concat "eww - " title) t)
					 (rename-buffer "eww" t))))))

  (defun aero/eww-open-in-new-buffer (url)
    "Fetch URL in a new EWW buffer."
    (interactive
     (let* ((uris (eww-suggested-uris))
            (prompt (concat "Enter URL or keywords"
                            (if uris (format " (default %s)" (car uris)) "")
                            ": ")))
       (list (read-string prompt nil nil uris))))
    (setq url (eww--dwim-expand-url url))
    (with-current-buffer
        (if (eq major-mode 'eww-mode) (clone-buffer)
          (generate-new-buffer "*eww*"))
      (unless (equal url (eww-current-url))
        (eww-mode)
        (eww (if (consp url) (car url) url)))))

  ;; normal browsing
  (evil-define-key 'normal eww-mode-map
    "?" 'describe-mode
    "^" 'eww-up-url
    "u" 'eww-up-url
    "U" 'eww-top-url
    (kbd "<backspace>") 'eww-back-url
    "H" 'eww-back-url
    "L" 'eww-forward-url
    "&" 'eww-browse-with-external-browser
    "d" 'eww-download
    "m" 'eww-add-bookmark
    "R" 'eww-readable
    "r" 'eww-reload
    "gr" 'eww-reload
    "T" 'aero/eww-open-in-new-buffer
    "q" 'kill-this-buffer
    "Q" 'quit-window
    "go" 'eww
    "gf" 'eww-view-source
    "gc" 'url-cookie-list
    "gh" 'eww-list-histories
    "gb" 'eww-list-buffers
    "gt" 'eww-list-buffers)

  ;; viewing history
  (evil-set-initial-state 'eww-history-mode 'normal)
  (evil-define-key 'normal eww-history-mode-map
    (kbd "RET") 'eww-history-browse
    "q" 'quit-window)

  ;; viewing buffers
  (evil-set-initial-state 'eww-buffers-mode 'normal)
  (evil-define-key 'normal eww-buffers-mode-map
    "D" 'eww-buffer-kill
    (kbd "RET") 'eww-buffer-select
    "q" 'quit-window)

  ;; bookmarks
  (evil-set-initial-state 'eww-bookmark-mode 'normal)
  (evil-define-key 'normal eww-bookmark-mode-map
    "D" 'eww-bookmark-kill
    "P" 'eww-bookmark-yank
    (kbd "RET") 'eww-bookmark-browse
    "q" 'quit-window))

(provide 'aero-eww)

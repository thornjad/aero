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

(require 'evil)

(setq browse-url-browser-function #'eww-browse-url)
(use-package eww
  :commands (eww
             eww-browse-url
             eww-search-words)
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
    "ws" '(eww-search-words :which-key "web search")
    "ww" 'eww)

	:config
	(setq eww-search-prefix "https://duckduckgo.com/lite?q=")

	(defun add-title-to-eww-buffer-name ()
		"Rename eww mode buffer so the title of the page is displayed, making
		 fake-tabbed-browsing easier"
		(let ((title (plist-get eww-data :title)))
			(when (eq major-mode 'eww-mode)
				(if title
						(rename-buffer (concat "eww - " title) t)
					(rename-buffer "eww" t)))))
	(add-hook 'eww-after-render-hook 'add-title-to-eww-buffer-name)

  ;; normal browsing
  (evil-define-key 'normal eww-mode-map
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
    "q" 'kill-buffer
    "Q" 'quit-window
    "go" 'eww
    "gf" 'eww-view-source
    "gc" 'url-cookie-list
    "gh" 'eww-list-histories
    "gb" 'eww-list-buffers
    "gt" 'eww-list-buffers)

  ;; viewing history
  (evil-set-initial-state 'normal 'eww-history-mode)
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

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(package! eww :builtin
  :after (general evil ace-link)
  :commands (eww eww-browse-url eww-search-words browse-url-at-point)

  :preface
  (defun aero/set-eww-buffer-title ()
    "Rename eww mode buffer so the title of the page is displayed, making
     fake-tabbed-browsing easier"
    (let ((title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode)
        (if title
            (rename-buffer (concat "eww - " title) t)
          (rename-buffer "eww" t)))))

  (defun aero/wiki-news () (interactive)
         (eww-browse-url "https://en.wikipedia.org/wiki/Portal:Current_events"))

  (defun aero/xwidgets-search-ddg (&optional term)
    (interactive "sSearch DuckDuckGo: ")
    (xwidget-webkit-browse-url (format "https://duckduckgo.com/?q=%s" (or term "")) t))

  (defun aero/ace-link-eww-new-buffer ()
    "Call `ace-link-eww' but open in a new buffer.

This simply calls `ace-link-eww' with a fake double prefix, which is equivalent to the list containing 16."
    (interactive)
    (ace-link-eww '(16)))

  (defun aero/pocket-add-current-url ()
    "Add the URL currently visited to Pocket."
    (interactive)
    (when (require 'pocket-reader nil t)
      (let ((url (eww-current-url)))
        (if (pocket-lib-add-urls url)
            (message "Added: %s" url)
          (message "Failed to add to Pocket")))))

  :hook ((eww-mode . visual-line-mode)
         (eww-after-render . aero/set-eww-buffer-title))

  :custom
  ;; Open everything in eww, except for these few sites which just don't work in eww
  (browse-url-browser-function
   '((".*google.*maps.*" . browse-url-generic)
     ("docs.google.com" . browse-url-generic)
     ("*.atlassian.com" . browse-url-generic)
     ("*.atlassian.net" . browse-url-generic)
     ("github.com" . browse-url-generic)
     ("gitlab.com" . browse-url-generic)
     ("melpa.org" . browse-url-generic)
     ("zoom.us" . browse-url-generic)
     ("t.co" . browse-url-generic)
     ("twitter.com" . browse-url-generic)
     ("youtube.com" . browse-url-generic)
     ("reddit.com" . browse-url-generic)
     ("." . eww-browse-url)))

  ;; MacOS needs its hand held to find the binary
  (browse-url-generic-program (if (system-is-mac)
                                  "/Applications/Firefox Developer Edition.app/Contents/MacOS/firefox"
                                "firefox"))

  (eww-search-prefix "https://lite.duckduckgo.com/lite?q=")
  (shr-max-width 90)
  (shr-indentation 2)

  :init
  (aero-leader-def "wbn" '(aero/wiki-news :wk "wikipedia news"))

  :config
  (evil-define-key 'normal eww-mode-map
    "SPC SPC" 'execute-extended-command
    "?" 'describe-mode
    "^" 'eww-up-url
    "u" 'eww-up-url
    "U" 'eww-top-url
    (kbd "<backspace>") 'eww-back-url
    "H" 'eww-back-url
    "L" 'eww-forward-url
    "&" 'eww-browse-with-external-browser
    "D" 'eww-download
    "o" 'eww
    "O" 'eww-open-in-new-buffer
    "p" 'pocket-reader-eww-add-link
    "P" 'aero/pocket-add-current-url
    "f" 'ace-link-eww
    "F" 'aero/ace-link-eww-new-buffer
    "m" 'eww-add-bookmark
    "R" 'eww-readable
    "r" 'eww-reload
    "gr" 'eww-reload
    "J" 'eww-buffer-show-next
    "K" 'eww-buffer-show-previous
    "T" 'eww-open-in-new-buffer
    "W" 'eww-copy-page-url
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

;; Add some org-like features
(package! shrface (:host github :repo "chenyanming/shrface")
  :defer t
  :after (eww)
  :hook (eww-after-render . shrface-mode)
  :custom (shrface-href-versatile t)
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings))

;; Add syntax highlighting to HTML pre tags
(package! shr-tag-pre-highlight "xuchunyang/shr-tag-pre-highlight.el"
  :after (shr)
  :config
  (add-to-list 'shr-external-rendering-functions '(pre . shr-tag-pre-highlight)))

(package! devdocs (:host github :repo "astoff/devdocs.el")
  :after (general)
  :commands (devdocs-lookup)
  :custom (devdocs-data-dir (expand-file-name "devdocs" aero-cache-dir))
  :init
  (aero-leader-def "hD" 'devdocs-lookup)
  (add-hook 'python-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'python-ts-mode-hook (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'typescript-mode-hook (lambda ()
                                    (setq-local devdocs-current-docs
                                                '("typescript" "rxjs" "angular" "javascript"))))
  (add-hook 'ng2-ts-mode-hook (lambda ()
                                (setq-local devdocs-current-docs
                                            '("typescript" "angular" "rxjs" "javascript" "html"))))
  (add-hook 'web-mode-hook (lambda ()
                             (setq-local devdocs-current-docs
                                         '("angular" "rxjs" "javascript" "html"))))
  (add-hook 'clojure-mode-hook (lambda ()
                                 (setq-local devdocs-current-docs
                                             '("clojure~1.11")))))

(provide 'aero-eww)

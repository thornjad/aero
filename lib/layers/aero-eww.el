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

(defun aero/ddg (&optional term)
  (interactive "sSearch DuckDuckGo: ")
  (eww-browse-url (format "https://lite.duckduckgo.com/lite?q=%s" (or term ""))))
(defun aero/wiki-news () (interactive)
       (eww-browse-url "https://en.wikipedia.org/wiki/Portal:Current_events"))

(defun aero/xwidgets-search-ddg (&optional term)
  (interactive "sSearch DuckDuckGo: ")
  (xwidget-webkit-browse-url (format "https://duckduckgo.com/?q=%s" (or term "")) t))

(package! eww :builtin
  :after (general evil ace-link)
  :commands (eww eww-browse-url eww-search-words browse-url-at-point)
  :init
  ;; Open everything in eww, except for these few sites which just don't work in eww
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ("docs.google.com" . browse-url-generic)
          ("*.atlassian.com" . browse-url-generic)
          ("*.atlassian.net" . browse-url-generic)
          ("http.*\/\/github.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("zoom.us" . browse-url-generic)
          ("t.co" . browse-url-generic)
          ("twitter.com" . browse-url-generic)
          ("youtube.com" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq browse-url-generic-program "firefox"
	      eww-search-prefix "https://lite.duckduckgo.com/lite?q="
        shr-max-width 90
        shr-indentation 2)

  ;; Hold mac's hand in finding binaries
  (when (system-is-mac)
    (setq browse-url-generic-program "/Applications/Firefox Developer Edition.app/Contents/MacOS/firefox"))

  (aero-leader-def
    "wbd" '(aero/ddg :wk "duckduckgo")
    "wbn" '(aero/wiki-news :wk "wikipedia news"))

	:config
  (package! shrface :auto
    :defer t
    :config
    (shrface-basic)
    (shrface-trial)
    (shrface-default-keybindings) ; setup default keybindings
    (setq shrface-href-versatile t))
  (add-hook 'eww-after-render-hook #'shrface-mode)

  (package! shr-tag-pre-highlight :auto
    :after shr
    :config
    (add-to-list 'shr-external-rendering-functions
                 '(pre . shr-tag-pre-highlight))
    (when (version< emacs-version "26")
      (with-eval-after-load 'eww
        (advice-add 'eww-display-html :around
                    'eww-display-html--override-shr-external-rendering-functions))))

  (add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'visual-line-mode)

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

  ;; normal browsing
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
    "s" 'aero/ddg
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

;; Hack to decrease jumpiness around images and scrolling
(with-eval-after-load "shr"
  (defun shr-put-image (spec alt &optional flags)
    "Insert image SPEC with a string ALT.  Return image.
SPEC is either an image data blob, or a list where the first
element is the data blob and the second element is the content-type.
Hack to use `insert-sliced-image' to avoid jerky image scrolling."
    (if (display-graphic-p)
        (let* ((size (cdr (assq 'size flags)))
               (data (if (consp spec)
                         (car spec)
                       spec))
               (content-type (and (consp spec)
                                  (cadr spec)))
               (start (point))
               (image (cond
                       ((eq size 'original)
                        (create-image data nil t :ascent 100
                                      :format content-type))
                       ((eq content-type 'image/svg+xml)
                        (create-image data 'svg t :ascent 100))
                       ((eq size 'full)
                        (ignore-errors
                          (shr-rescale-image data content-type
                                             (plist-get flags :width)
                                             (plist-get flags :height))))
                       (t
                        (ignore-errors
                          (shr-rescale-image data content-type
                                             (plist-get flags :width)
                                             (plist-get flags :height)))))))
          (when image
            (let* ((image-pixel-cons (image-size image t))
                   (image-pixel-width (car image-pixel-cons))
                   (image-pixel-height (cdr image-pixel-cons))
                   (image-scroll-rows (round (/ image-pixel-height (default-font-height)))))
              ;; When inserting big-ish pictures, put them at the
              ;; beginning of the line.
              (when (and (> (current-column) 0)
                         (> (car (image-size image t)) 400))
                (insert "\n"))

              (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
              ;; (if (eq size 'original)
              ;;     (insert-sliced-image image (or alt "*") nil image-scroll-rows 1)
              ;;   (insert-image image (or alt "*")))

              (put-text-property start (point) 'image-size size)
              (when (and shr-image-animate
                         (cond ((fboundp 'image-multi-frame-p)
                                ;; Only animate multi-frame things that specify a
                                ;; delay; eg animated gifs as opposed to
                                ;; multi-page tiffs.  FIXME?
                                (cdr (image-multi-frame-p image)))
                               ((fboundp 'image-animated-p)
                                (image-animated-p image))))
                (image-animate image nil 60))))
          image)
      (insert (or alt "")))))

(package! devdocs :auto
  :after (general)
  :commands (devdocs-lookup)
  :custom (devdocs-data-dir (expand-file-name "devdocs" aero-cache-dir))
  :init
  (aero-leader-def "hD" 'devdocs-lookup)
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'python-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.12"))))
  (add-hook 'typescript-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript" "rxjs" "angular" "javascript"))))
  (add-hook 'ng2-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript" "angular" "rxjs" "javascript" "html"))))
  (add-hook 'web-mode-hook
            (lambda () (setq-local devdocs-current-docs '("angular" "rxjs" "javascript" "html")))))

(provide 'aero-eww)

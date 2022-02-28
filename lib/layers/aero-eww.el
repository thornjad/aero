;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2022 Jade Michael Thornton
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
(defun aero/wiki (&optional term)
  (interactive "sSearch Wikipedia: ")
  (aero/ddg (format "!w %s" (or term ""))))
(defun aero/wiki-news () (interactive)
       (eww-browse-url "https://en.wikipedia.org/wiki/Portal:Current_events"))
(defun aero/npr-news () (interactive)
       (eww-browse-url "https://text.npr.org/"))

(use-package eww :straight nil
  :after (general evil ace-link)
  :commands (eww
             eww-browse-url
             eww-search-words
             browse-url-at-point)
  :init
  ;; Open everything in eww, except for these few sites which just don't work in eww
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ("docs.google.com" . browse-url-generic)
          ("http.*\/\/github.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("zoom.us" . browse-url-generic)
          ("t.co" . browse-url-generic)
          ("twitter.com" . browse-url-generic)
          ("youtube.com" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq browse-url-generic-program "firefox"
	      eww-search-prefix "https://lite.duckduckgo.com/lite?q="
        shr-max-width 100
        shr-indentation 2)

  (when (system-is-mac)
    (setq browse-url-generic-program "/Applications/Firefox Developer Edition.app/Contents/MacOS/firefox"))

  (aero-leader-def
    "wbd" '(aero/ddg :wk "duckduckgo")
    "wbw" '(aero/wiki :wk "wikipedia")
    "wbn" '(:ignore t :wk "news sites")
    "wbnw" '(aero/wiki-news :wk "wikipedia")
    "wbnn" '(aero/npr-news :wk "npr"))

	:config
  (use-package shr-tag-pre-highlight :straight t
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
    "D" 'eww-download
    "o" 'eww
    "O" 'aero/ddg
    "f" 'ace-link-eww
    "m" 'eww-add-bookmark
    "R" 'eww-readable
    "r" 'eww-reload
    "gr" 'eww-reload
    "J" 'eww-buffer-show-next
    "K" 'eww-buffer-show-previous
    "T" 'aero/eww-open-in-new-buffer
    "q" 'kill-this-buffer
    "Q" 'quit-window
    "go" 'eww
    "gf" 'eww-view-source
    "gc" 'url-cookie-list
    "gh" 'eww-list-histories
    "gb" 'eww-list-buffers
    "gt" 'eww-list-buffers
    ;; Doesn't work:
    (kbd "M-h") 'windmove-left
    (kbd "M-l") 'windmove-right
    ;; these actually work, but suck:
    (kbd "M-H") 'windmove-left
    (kbd "M-L") 'windmove-right)

  ;; Doesn't work:
  (when (require 'bind-key nil t)
    (bind-keys :map eww-mode-map
      ("M-h" . windmove-left)
      ("M-j" . windmove-down)
      ("M-k" . windmove-up)
      ("M-l" . windmove-right)))

  ;; Doesn't work:
  (when (require 'bind-key nil t)
    (bind-keys :map outline-mode-map
      ("M-h" . windmove-left)
      ("M-j" . windmove-down)
      ("M-k" . windmove-up)
      ("M-l" . windmove-right)))

  ;; trying to override this, but it doesnt work
  (with-eval-after-load 'outline-mode
    (define-key outline-mode-map (kbd "M-h") 'windmove-left)
    (define-key outline-mode-map (kbd "M-l") 'windmove-right))

  ;; Doesn't work:
  (defun aero/eww-please ()
    "Please, eww, let me bind these."
    (define-key eww-mode-map (kbd "M-h") 'windmove-left)
    (define-key eww-mode-map (kbd "M-l") 'windmove-right)
    (evil-define-key 'normal eww-mode-map
      (kbd "M-h") 'windmove-left
      (kbd "M-l") 'windmove-right))
  (aero/eww-please)
  (add-hook 'eww-after-render-hook #'aero/eww-please)

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

;; Hack to decrease jumpiness around images and scrolling(with-eval-after-load "shr"
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

(use-package pocket-reader :straight t
  :after (general)
  :commands (pocket-reader)
  :custom
  (pocket-reader-open-url-default-function #'eww)
  :init (aero-leader-def "aP" 'pocket-reader)
  :config
  ;; Evil messes with all the bindings, so we'll use the defaults in emacs mode.
  (evil-set-initial-state 'pocket-reader-mode 'emacs))

(use-package hnreader :straight t
  :after (general)
  :commands (hnreader-news)
  :init (aero-leader-def "wn" 'hnreader-news))

(use-package counsel-web :straight t
  :after (general)
  :commands (counsel-web-suggest counsel-web-search)
  :init
  (aero-leader-def
    "ww" 'counsel-web-suggest
    "ws" 'counsel-web-search))

(use-package howdoyou :straight t
  :after (general counsel-web)
  :commands (howdoyou-query)
  :custom
  (howdoyou-switch-to-answer-buffer t)
  :init
  (defun aero/howdoyou-with-suggestions ()
    "Call `howdoyou-query' with suggestions from `counsel-web-suggest'."
    (interactive)
    (counsel-web-suggest nil
                         "How Do You: "
                         #'counsel-web-suggest--duckduckgo
                         (lambda (x)
                           (howdoyou-query x))))
  (aero-leader-def "hs" 'aero/howdoyou-with-suggestions)

  (aero-mode-leader-def 'howdoyou-mode-map
    "n" 'howdoyou-next-link
    "p" 'howdoyou-previous-link
    "P" 'howdoyou-go-back-to-first-link))

(use-package devdocs :straight t
  :after (general)
  :commands (devdocs-lookup)
  :init
  (aero-leader-def "hD" 'devdocs-lookup)
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("python~3.10"))))
  (add-hook 'typescript-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript" "rxjs" "angular" "javascript"))))
  (add-hook 'ng2-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("typescript" "angular" "rxjs" "javascript"))))
  (add-hook 'web-mode-hook
            (lambda () (setq-local devdocs-current-docs '("angular" "rxjs" "javascript")))))

(provide 'aero-eww)

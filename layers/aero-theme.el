;; -*- lexical-binding: t -*-
;; Aero Theme
;;
;; Copyright (c) 2019 Jade Michael Thornton
;; Copyright (c) 2017 Martijn Terpstra
;; Copyright (c) 2016-2017 Jason Milkins
;; Copyright (c) 2013-2016 Eduardo Lavaque
;; Copyright (c) 2013 Lee Machin
;;
;; Package-Requires: ((autothemer "0.2"))
;;
;; Commentary:
;;
;; Aero theme is based on Gruvbox, a retro-groove theme for Emacs. While many of
;; the original faces remain the same as Gruvbox, this is no longer a simple
;; fork.
;;
;; Pavel Pertsev created the original Gruvbox theme for Vim. Lee Machin create
;; the first Emacs port, further developed by Greduan. Jason Milkins has
;; maintained the Emacs Gruvbox theme since 2015, working to align the project
;; closely with the Vim project. Martijn Terpstra helped re-implement the theme
;; with autothemer, and provided a large amount of mode enhancements. Jade
;; Michael Thornton forked the Gruvbox project in 2019 to develop the Aero
;; theme.
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs
;;
;; Code:

(eval-when-compile
  (require 'cl-lib))

(use-package autothemer :ensure t)

(defmacro aero-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette
    ((default                                   (:background aero-bg :foreground aero-light0))
     (cursor                                    (:background aero-light0))
     (mode-line                                 (:background aero-dark3 :foreground aero-light2 :box nil))
     (mode-line-inactive                        (:background aero-dark1 :foreground aero-light4 :box nil))
     (fringe                                    (:background aero-bg))
     (hl-line                                   (:background aero-dark1))
     (region                                    (:background aero-dark2))
     (secondary-selection                       (:background aero-dark1))
     (minibuffer-prompt                         (:background aero-bg :foreground aero-bright_green :bold t))
     (vertical-border                           (:foreground aero-dark2))
     (window-divider                            (:foreground aero-dark2))
     (link                                      (:foreground aero-faded_blue :underline t))
     (shadow                                    (:foreground aero-dark4))
     (page-break-lines                          (:foreground aero-dark4))

     ;; Built-in syntax
     (font-lock-builtin-face                            (:foreground aero-bright_orange))
     (font-lock-constant-face                           (:foreground aero-bright_purple))
     (font-lock-comment-face                            (:foreground aero-dark4))
     (font-lock-function-name-face                      (:foreground aero-bright_yellow))
     (font-lock-keyword-face                            (:foreground aero-bright_red))
     (font-lock-string-face                             (:foreground aero-bright_green))
     (font-lock-variable-name-face                      (:foreground aero-bright_blue))
     (font-lock-type-face                               (:foreground aero-bright_purple))
     (font-lock-warning-face                            (:foreground aero-bright_red :bold t))

     ;; Basic faces
     (error                                             (:foreground aero-bright_red :bold t))
     (success                                           (:foreground aero-bright_green :bold t))
     (warning                                           (:foreground aero-bright_yellow :bold t))
     (trailing-whitespace                               (:background aero-bright_red))
     (escape-glyph                                      (:foreground aero-bright_aqua))
     (header-line                                       (:background aero-dark0 :foreground aero-light3 :box nil :inherit nil))
     (highlight                                         (:background aero-dark4 :foreground aero-light0))
     (homoglyph                                         (:foreground aero-bright_yellow))
     (match                                             (:foreground aero-dark0 :background aero-bright_blue))

     ;; Customize faces
     (widget-field                                      (:background aero-dark3))
     (custom-group-tag                                  (:foreground aero-bright_blue :weight 'bold))
     (custom-variable-tag                               (:foreground aero-bright_blue :weight 'bold))

     ;; whitespace-mode
     (whitespace-space                          (:background aero-bg :foreground aero-dark4))
     (whitespace-hspace                         (:background aero-bg :foreground aero-dark4))
     (whitespace-tab                            (:background aero-bg :foreground aero-dark4))
     (whitespace-newline                        (:background aero-bg :foreground aero-dark4))
     (whitespace-trailing                       (:background aero-dark1 :foreground aero-bright_red))
     (whitespace-line                           (:background aero-dark1 :foreground aero-bright_red))
     (whitespace-space-before-tab               (:background aero-bg :foreground aero-dark4))
     (whitespace-indentation                    (:background aero-bg :foreground aero-dark4))
     (whitespace-empty                          (:background nil :foreground nil))
     (whitespace-space-after-tab                (:background aero-bg :foreground aero-dark4))

     ;; Highlight indentation mode
     (highlight-indentation-current-column-face (:background aero-dark2))
     (highlight-indentation-face                (:background aero-dark1))

     ;; smartparens
     (sp-pair-overlay-face                      (:background aero-dark2))
     (sp-show-pair-match-face                   (:background aero-dark2)) ;; Pair tags highlight
     (sp-show-pair-mismatch-face                (:background aero-bright_red)) ;; Highlight for bracket without pair

     ;; elscreen
     (elscreen-tab-background-face              (:background aero-bg :box nil)) ;; Tab bar, not the tabs
     (elscreen-tab-control-face                 (:background aero-dark2 :foreground aero-bright_red :underline nil :box nil)) ;; The controls
     (elscreen-tab-current-screen-face          (:background aero-dark4 :foreground aero-dark0 :box nil)) ;; Current tab
     (elscreen-tab-other-screen-face            (:background aero-dark2 :foreground aero-light4 :underline nil :box nil)) ;; Inactive tab

     ;; ag (The Silver Searcher)
     (ag-hit-face                               (:foreground aero-bright_blue))
     (ag-match-face                             (:foreground aero-bright_red))

     ;; Diffs
     (diff-changed                              (:background nil :foreground aero-light1))
     (diff-added                                (:background nil :foreground aero-bright_green))
     (diff-removed                              (:background nil :foreground aero-bright_red))
     (diff-indicator-changed                    (:inherit 'diff-changed))
     (diff-indicator-added                      (:inherit 'diff-added))
     (diff-indicator-removed                    (:inherit 'diff-removed))

     (js2-warning                               (:underline (:color aero-bright_yellow :style 'wave)))
     (js2-error                                 (:underline (:color aero-bright_red :style 'wave)))
     (js2-external-variable                     (:underline (:color aero-bright_aqua :style 'wave)))
     (js2-jsdoc-tag                             (:background nil :foreground aero-gray  ))
     (js2-jsdoc-type                            (:background nil :foreground aero-light4))
     (js2-jsdoc-value                           (:background nil :foreground aero-light3))
     (js2-function-param                        (:background nil :foreground aero-bright_aqua))
     (js2-function-call                         (:background nil :foreground aero-bright_blue))
     (js2-instance-member                       (:background nil :foreground aero-bright_orange))
     (js2-private-member                        (:background nil :foreground aero-faded_yellow))
     (js2-private-function-call                 (:background nil :foreground aero-faded_aqua))
     (js2-jsdoc-html-tag-name                   (:background nil :foreground aero-light4))
     (js2-jsdoc-html-tag-delimiter              (:background nil :foreground aero-light3))

     ;; popup
     (popup-face                                (:underline nil :foreground aero-light1 :background aero-dark1))
     (popup-menu-mouse-face                     (:underline nil :foreground aero-light0 :background aero-faded_green))
     (popup-menu-selection-face                 (:underline nil :foreground aero-light0 :background aero-faded_green))
     (popup-tip-face                            (:underline nil :foreground aero-light2 :background aero-dark2))

     ;; counsel
     (counsel-M-x-key                              (:foreground aero-bright_orange ))
     (counsel-action                               (:foreground aero-light0_hard :underline t))
     (counsel-bookmark-addressbook                 (:foreground aero-bright_red))
     (counsel-bookmark-directory                   (:foreground aero-bright_purple))
     (counsel-bookmark-file                        (:foreground aero-faded_blue))
     (counsel-bookmark-gnus                        (:foreground aero-faded_purple))
     (counsel-bookmark-info                        (:foreground aero-turquoise4))
     (counsel-bookmark-man                         (:foreground aero-sienna))
     (counsel-bookmark-w3m                         (:foreground aero-bright_yellow))
     (counsel-buffer-directory                     (:foreground aero-white :background aero-bright_blue))
     (counsel-buffer-not-saved                     (:foreground aero-faded_red))
     (counsel-buffer-process                       (:foreground aero-burlywood4))
     (counsel-buffer-saved-out                     (:foreground aero-bright_red))
     (counsel-buffer-size                          (:foreground aero-bright_purple))
     (counsel-candidate-number                     (:foreground aero-bright_green))
     (counsel-ff-directory                         (:foreground aero-bright_purple))
     (counsel-ff-executable                        (:foreground aero-turquoise4))
     (counsel-ff-file                              (:foreground aero-sienna))
     (counsel-ff-invalid-symlink                   (:foreground aero-white :background aero-bright_red))
     (counsel-ff-prefix                            (:foreground aero-black :background aero-bright_yellow))
     (counsel-ff-symlink                           (:foreground aero-bright_orange))
     (counsel-grep-cmd-line                        (:foreground aero-bright_green))
     (counsel-grep-file                            (:foreground aero-faded_purple))
     (counsel-grep-finish                          (:foreground aero-turquoise4))
     (counsel-grep-lineno                          (:foreground aero-bright_orange))
     (counsel-grep-match                           (:foreground aero-bright_yellow))
     (counsel-grep-running                         (:foreground aero-bright_red))
     (counsel-header                               (:foreground aero-aquamarine4))
     (counsel-helper                               (:foreground aero-aquamarine4))
     (counsel-history-deleted                      (:foreground aero-black :background aero-bright_red))
     (counsel-history-remote                       (:foreground aero-faded_red))
     (counsel-lisp-completion-info                 (:foreground aero-faded_orange))
     (counsel-lisp-show-completion                 (:foreground aero-bright_red))
     (counsel-locate-finish                        (:foreground aero-white :background aero-aquamarine4))
     (counsel-match                                (:foreground aero-bright_orange))
     (counsel-moccur-buffer                        (:foreground aero-bright_aqua :underline t))
     (counsel-prefarg                              (:foreground aero-turquoise4))
     (counsel-selection                            (:foreground aero-white :background aero-dark2))
     (counsel-selection-line                       (:foreground aero-white :background aero-dark2))
     (counsel-separator                            (:foreground aero-faded_red))
     (counsel-source-header                        (:foreground aero-light2))
     (counsel-visible-mark                         (:foreground aero-black :background aero-light3))

     ;;hi-lock-mode
     (hi-black-b                                (:foreground aero-black :weight 'bold))
     (hi-black-hb                               (:foreground aero-black :weight 'bold :height 1.5))
     (hi-blue                                   (:foreground aero-dark0 :background aero-bright_blue))
     (hi-blue-b                                 (:foreground aero-bright_blue :weight 'bold))
     (hi-green                                  (:foreground aero-dark0 :background aero-bright_green))
     (hi-green-b                                (:foreground aero-bright_green :weight 'bold))
     (hi-pink                                   (:foreground aero-dark0 :background aero-bright_purple))
     (hi-red-b                                  (:foreground aero-bright_red :weight 'bold))
     (hi-yellow                                 (:foreground aero-dark0 :background aero-faded_yellow))

     ;; company-mode
     (company-scrollbar-bg                      (:background aero-dark1))
     (company-scrollbar-fg                      (:background aero-dark0_soft))
     (company-tooltip                           (:background aero-dark0_soft))
     (company-tooltip-annotation                (:foreground aero-bright_green))
     (company-tooltip-annotation-selection      (:inherit 'company-tooltip-annotation))
     (company-tooltip-selection                 (:foreground aero-bright_purple :background aero-dark2))
     (company-tooltip-common                    (:foreground aero-bright_blue :underline t))
     (company-tooltip-common-selection          (:foreground aero-bright_blue :underline t))
     (company-preview-common                    (:foreground aero-light0))
     (company-preview                           (:background aero-lightblue4))
     (company-preview-search                    (:background aero-turquoise4))
     (company-template-field                    (:foreground aero-black :background aero-bright_yellow))
     (company-echo-common                       (:foreground aero-faded_red))

     ;; tool tips
     (tooltip                                   (:foreground aero-light1 :background aero-dark1))

     ;; term
     (term-color-black                          (:foreground aero-dark2 :background aero-dark1))
     (term-color-blue                           (:foreground aero-bright_blue :background aero-bright_blue))
     (term-color-cyan                           (:foreground aero-bright_aqua :background aero-bright_aqua))
     (term-color-green                          (:foreground aero-bright_green :background aero-bright_green))
     (term-color-magenta                        (:foreground aero-bright_purple :background aero-bright_purple))
     (term-color-red                            (:foreground aero-bright_red :background aero-bright_red))
     (term-color-white                          (:foreground aero-light1 :background aero-light1))
     (term-color-yellow                         (:foreground aero-bright_yellow :background aero-bright_yellow))
     (term-default-fg-color                     (:foreground aero-light0))
     (term-default-bg-color                     (:background aero-bg))

     ;; message-mode
     (message-header-to                         (:inherit 'font-lock-variable-name-face))
     (message-header-cc                         (:inherit 'font-lock-variable-name-face))
     (message-header-subject                    (:foreground aero-bright_orange :weight 'bold))
     (message-header-newsgroups                 (:foreground aero-bright_yellow :weight 'bold))
     (message-header-other                      (:inherit 'font-lock-variable-name-face))
     (message-header-name                       (:inherit 'font-lock-keyword-face))
     (message-header-xheader                    (:foreground aero-faded_blue))
     (message-separator                         (:inherit 'font-lock-comment-face))
     (message-cited-text                        (:inherit 'font-lock-comment-face))
     (message-mml                               (:foreground aero-faded_green :weight 'bold))

     ;; org-mode
     (org-hide                                  (:foreground aero-dark0))
     (org-level-1                               (:foreground aero-bright_blue))
     (org-level-2                               (:foreground aero-bright_yellow))
     (org-level-3                               (:foreground aero-bright_purple))
     (org-level-4                               (:foreground aero-bright_red))
     (org-level-5                               (:foreground aero-bright_green))
     (org-level-6                               (:foreground aero-bright_aqua))
     (org-level-7                               (:foreground aero-faded_blue))
     (org-level-8                               (:foreground aero-bright_orange))
     (org-special-keyword                       (:inherit 'font-lock-comment-face))
     (org-drawer                                (:inherit 'font-lock-function-name-face))
     (org-column                                (:background aero-dark0))
     (org-column-title                          (:background aero-dark0 :underline t :weight 'bold))
     (org-warning                               (:foreground aero-bright_red :weight 'bold :underline nil :bold t))
     (org-archived                              (:foreground aero-light0 :weight 'bold))
     (org-link                                  (:foreground aero-faded_aqua :underline t))
     (org-footnote                              (:foreground aero-bright_aqua :underline t))
     (org-ellipsis                              (:foreground aero-light4))
     (org-date                                  (:foreground aero-bright_blue :underline t))
     (org-sexp-date                             (:foreground aero-faded_blue :underline t))
     (org-tag                                   (:bold t :weight 'bold))
     (org-list-dt                               (:bold t :weight 'bold))
     (org-todo                                  (:foreground aero-bright_red :weight 'bold :bold t))
     (org-done                                  (:foreground aero-bright_aqua :weight 'bold :bold t))
     (org-agenda-done                           (:foreground aero-bright_aqua))
     (org-headline-done                         (:foreground aero-bright_aqua))
     (org-table                                 (:foreground aero-bright_blue))
     (org-block                                 (:background aero-dark0_soft))
     (org-block-begin-line                      (:background aero-dark1))
     (org-block-end-line                        (:background aero-dark1))
     (org-formula                               (:foreground aero-bright_yellow))
     (org-document-title                        (:foreground aero-faded_blue))
     (org-document-info                         (:foreground aero-faded_blue))
     (org-agenda-structure                      (:inherit 'font-lock-comment-face))
     (org-agenda-date-today                     (:foreground aero-light0 :weight 'bold :italic t))
     (org-scheduled                             (:foreground aero-bright_yellow))
     (org-scheduled-today                       (:foreground aero-bright_blue))
     (org-scheduled-previously                  (:foreground aero-faded_red))
     (org-upcoming-deadline                     (:inherit 'font-lock-keyword-face))
     (org-deadline-announce                     (:foreground aero-faded_red))
     (org-time-grid                             (:foreground aero-faded_orange))
     (org-latex-and-related                     (:foreground aero-bright_blue))

     ;; org-habit
     (org-habit-clear-face                      (:background aero-faded_blue))
     (org-habit-clear-future-face               (:background aero-bright_blue))
     (org-habit-ready-face                      (:background aero-faded_green))
     (org-habit-ready-future-face               (:background aero-bright_green))
     (org-habit-alert-face                      (:background aero-faded_yellow))
     (org-habit-alert-future-face               (:background aero-bright_yellow))
     (org-habit-overdue-face                    (:background aero-faded_red))
     (org-habit-overdue-future-face             (:background aero-bright_red))

     ;; elfeed
     (elfeed-search-title-face                  (:foreground aero-gray  ))
     (elfeed-search-unread-title-face           (:foreground aero-light0))
     (elfeed-search-date-face                   (:inherit 'font-lock-builtin-face :underline t))
     (elfeed-search-feed-face                   (:inherit 'font-lock-variable-name-face))
     (elfeed-search-tag-face                    (:inherit 'font-lock-keyword-face))
     (elfeed-search-last-update-face            (:inherit 'font-lock-comment-face))
     (elfeed-search-unread-count-face           (:inherit 'font-lock-comment-face))
     (elfeed-search-filter-face                 (:inherit 'font-lock-string-face))

     ;; markdown-mode
     (markdown-header-face-1                    (:foreground aero-bright_blue))
     (markdown-header-face-2                    (:foreground aero-bright_yellow))
     (markdown-header-face-3                    (:foreground aero-bright_purple))
     (markdown-header-face-4                    (:foreground aero-bright_red))
     (markdown-header-face-5                    (:foreground aero-bright_green))
     (markdown-header-face-6                    (:foreground aero-bright_aqua))

     ;; ace-jump-mode
     (ace-jump-face-background                  (:foreground aero-light4 :background aero-bg :inverse-video nil))
     (ace-jump-face-foreground                  (:foreground aero-bright_red :background aero-bg :inverse-video nil))

     ;; ace-window
     (aw-background-face                        (:forground  aero-light1 :background aero-bg :inverse-video nil))
     (aw-leading-char-face                      (:foreground aero-bright_red :background aero-bg :height 4.0))

     ;; show-paren
     (show-paren-match                          (:background aero-dark3 :foreground aero-bright_blue  :weight 'bold))
     (show-paren-mismatch                       (:background aero-bright_red :foreground aero-dark3 :weight 'bold))

     ;; ivy
     (ivy-current-match                         (:foreground aero-light0_hard :weight 'bold :underline t))
     (ivy-minibuffer-match-face-1               (:foreground aero-bright_orange))
     (ivy-minibuffer-match-face-2               (:foreground aero-bright_yellow))
     (ivy-minibuffer-match-face-3               (:foreground aero-faded_orange))
     (ivy-minibuffer-match-face-4               (:foreground aero-faded_yellow))

     ;; ido
     (ido-only-match                            (:foreground aero-faded_green))
     (ido-first-match                           (:foreground aero-faded_green))
     (ido-subdir                                (:foreground aero-faded_red))

     ;; magit
     (magit-bisect-bad                          (:foreground aero-faded_red))
     (magit-bisect-good                         (:foreground aero-faded_green))
     (magit-bisect-skip                         (:foreground aero-faded_yellow))
     (magit-blame-heading                       (:foreground aero-light0 :background aero-dark2))
     (magit-branch-local                        (:foreground aero-bright_blue))
     (magit-branch-current                      (:underline aero-bright_blue :inherit 'magit-branch-local))
     (magit-branch-remote                       (:foreground aero-bright_green))
     (magit-cherry-equivalent                   (:foreground aero-bright_purple))
     (magit-cherry-unmatched                    (:foreground aero-bright_aqua))
     (magit-diff-added                          (:foreground aero-bright_green))
     (magit-diff-added-highlight                (:foreground aero-bright_green :inherit 'magit-diff-context-highlight))
     (magit-diff-base                           (:background aero-faded_yellow :foreground aero-light2))
     (magit-diff-base-highlight                 (:background aero-faded_yellow :foreground aero-light0))
     (magit-diff-context                        (:foreground aero-dark1  :foreground aero-light1))
     (magit-diff-context-highlight              (:background aero-dark1 :foreground aero-light0))
     (magit-diff-hunk-heading                   (:background aero-dark3 :foreground aero-light2))
     (magit-diff-hunk-heading-highlight         (:background aero-dark2 :foreground aero-light0))
     (magit-diff-hunk-heading-selection         (:background aero-dark2 :foreground aero-bright_orange))
     (magit-diff-lines-heading                  (:background aero-faded_orange :foreground aero-light0))
     (magit-diff-removed                        (:foreground aero-bright_red))
     (magit-diff-removed-highlight              (:foreground aero-bright_red :inherit 'magit-diff-context-highlight))
     (magit-diffstat-added                      (:foreground aero-faded_green))
     (magit-diffstat-removed                    (:foreground aero-faded_red))
     (magit-dimmed                              (:foreground aero-dark4))
     (magit-hash                                (:foreground aero-bright_blue))
     (magit-log-author                          (:foreground aero-bright_red))
     (magit-log-date                            (:foreground aero-bright_aqua))
     (magit-log-graph                           (:foreground aero-dark4))
     (magit-process-ng                          (:foreground aero-bright_red :weight 'bold))
     (magit-process-ok                          (:foreground aero-bright_green :weight 'bold))
     (magit-reflog-amend                        (:foreground aero-bright_purple))
     (magit-reflog-checkout                     (:foreground aero-bright_blue))
     (magit-reflog-cherry-pick                  (:foreground aero-bright_green))
     (magit-reflog-commit                       (:foreground aero-bright_green))
     (magit-reflog-merge                        (:foreground aero-bright_green))
     (magit-reflog-other                        (:foreground aero-bright_aqua))
     (magit-reflog-rebase                       (:foreground aero-bright_purple))
     (magit-reflog-remote                       (:foreground aero-bright_blue))
     (magit-reflog-reset                        (:foreground aero-bright_red))
     (magit-refname                             (:foreground aero-light4))
     (magit-section-heading                     (:foreground aero-bright_yellow :weight 'bold))
     (magit-section-heading-selection           (:foreground aero-faded_yellow))
     (magit-section-highlight                   (:background aero-dark1))
     (magit-sequence-drop                       (:foreground aero-faded_yellow))
     (magit-sequence-head                       (:foreground aero-bright_aqua))
     (magit-sequence-part                       (:foreground aero-bright_yellow))
     (magit-sequence-stop                       (:foreground aero-bright_green))
     (magit-signature-bad                       (:foreground aero-bright_red :weight 'bold))
     (magit-signature-error                     (:foreground aero-bright_red))
     (magit-signature-expired                   (:foreground aero-bright_orange))
     (magit-signature-good                      (:foreground aero-bright_green))
     (magit-signature-revoked                   (:foreground aero-bright_purple))
     (magit-signature-untrusted                 (:foreground aero-bright_blue))
     (magit-tag                                 (:foreground aero-bright_yellow))

     ;; git-gutter
     (git-gutter:modified                       (:background aero-faded_blue :foreground aero-faded_blue))
     (git-gutter:added                          (:background aero-faded_green :foreground aero-faded_green))
     (git-gutter:deleted                        (:background aero-faded_red :foreground aero-faded_red))

     ;; git-gutter+
     (git-gutter+-modified                      (:foreground aero-faded_blue :background aero-faded_blue))
     (git-gutter+-added                         (:foreground aero-faded_green :background aero-faded_green))
     (git-gutter+-deleted                       (:foreground aero-faded_red :background aero-faded_red))

     ;; git-gutter-fringe
     (git-gutter-fr:modified                    (:inherit 'git-gutter:modified))
     (git-gutter-fr:added                       (:inherit 'git-gutter:added))
     (git-gutter-fr:deleted                     (:inherit 'git-gutter:deleted))

     ;; flyspell
     (flyspell-duplicate                        (:underline (:color aero-light4 :style 'line)))
     (flyspell-incorrect                        (:underline (:color aero-bright_red :style 'line)))

     ;; langtool
     (langtool-errline                          (:foreground aero-dark0 :background aero-bright_red))
     (langtool-correction-face                  (:foreground aero-bright_yellow :weight 'bold))

     ;; latex
     (font-latex-bold-face                      (:foreground aero-faded_green :bold t))
     (font-latex-italic-face                    (:foreground aero-bright_green :underline t))
     (font-latex-math-face                      (:foreground aero-light3))
     (font-latex-script-char-face               (:foreground aero-faded_aqua))
     (font-latex-sectioning-5-face              (:foreground aero-bright_yellow :bold t))
     (font-latex-sedate-face                    (:foreground aero-light4))
     (font-latex-string-face                    (:foreground aero-bright_orange))
     (font-latex-verbatim-face                  (:foreground aero-light4))
     (font-latex-warning-face                   (:foreground aero-bright_red :weight 'bold))
     (preview-face                              (:background aero-dark1))

     ;; mu4e
     (mu4e-header-key-face                      (:foreground aero-bright_green :weight 'bold ))
     (mu4e-unread-face                          (:foreground aero-bright_blue :weight 'bold ))
     (mu4e-highlight-face                       (:foreground aero-bright_green))

     ;; shell script
     (sh-quoted-exec                            (:foreground aero-bright_purple))
     (sh-heredoc                                (:foreground aero-bright_orange))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face   (:foreground aero-light0))
     (undo-tree-visualizer-current-face         (:foreground aero-bright_red))
     (undo-tree-visualizer-default-face         (:foreground aero-dark4))
     (undo-tree-visualizer-register-face        (:foreground aero-bright_yellow))
     (undo-tree-visualizer-unmodified-face      (:foreground aero-bright_aqua))

     ;; widget faces
     (widget-button-pressed-face                (:foreground aero-bright_red))
     (widget-documentation-face                 (:foreground aero-faded_green))
     (widget-field                              (:foreground aero-light0 :background aero-dark2))
     (widget-single-line-field                  (:foreground aero-light0 :background aero-dark2))

     ;; dired+
     (diredp-file-name                          (:foreground aero-light2))
     (diredp-file-suffix                        (:foreground aero-light4))
     (diredp-compressed-file-suffix             (:foreground aero-faded_blue))
     (diredp-dir-name                           (:foreground aero-faded_blue))
     (diredp-dir-heading                        (:foreground aero-bright_blue))
     (diredp-symlink                            (:foreground aero-bright_orange))
     (diredp-date-time                          (:foreground aero-light3))
     (diredp-number                             (:foreground aero-faded_blue))
     (diredp-no-priv                            (:foreground aero-dark4))
     (diredp-other-priv                         (:foreground aero-dark2))
     (diredp-rare-priv                          (:foreground aero-dark4))
     (diredp-ignored-file-name                  (:foreground aero-dark4))

     (diredp-dir-priv                           (:foreground aero-faded_blue  :background aero-dark_blue))
     (diredp-exec-priv                          (:foreground aero-faded_blue  :background aero-dark_blue))
     (diredp-link-priv                          (:foreground aero-faded_aqua  :background aero-dark_aqua))
     (diredp-read-priv                          (:foreground aero-bright_red  :background aero-dark_red))
     (diredp-write-priv                         (:foreground aero-bright_aqua :background aero-dark_aqua))

     ;; neotree
     (neo-banner-face                           (:foreground aero-bright_purple :bold t))
     (neo-dir-link-face                         (:foreground aero-bright_yellow))
     (neo-expand-btn-face                       (:foreground aero-bright_orange))
     (neo-file-link-face                        (:foreground aero-light0))
     (neo-header-face                           (:foreground aero-bright_purple))
     (neo-root-dir-face                         (:foreground aero-bright_purple :bold t))

     ;; eshell
     (eshell-prompt-face                         (:foreground aero-bright_aqua))
     (eshell-ls-archive-face                     (:foreground aero-light3))
     (eshell-ls-backup-face                      (:foreground aero-light4))
     (eshell-ls-clutter-face                     (:foreground aero-bright_orange :weight 'bold))
     (eshell-ls-directory-face                   (:foreground aero-bright_yellow))
     (eshell-ls-executable-face                  (:weight 'bold))
     (eshell-ls-missing-face                     (:foreground aero-bright_red :bold t))
     (eshell-ls-product-face                     (:foreground aero-faded_red))
     (eshell-ls-readonly-face                    (:foreground aero-light2))
     (eshell-ls-special-face                     (:foreground aero-bright_yellow :bold t))
     (eshell-ls-symlink-face                     (:foreground aero-bright_red))
     (eshell-ls-unreadable-face                  (:foreground aero-bright_red :bold t))

     ;; wgrep
     (wgrep-delete-face                          (:strike-through aero-bright_red))
     (wgrep-done-face                            (:foreground aero-turquoise4))
     (wgrep-face                                 (:underline (:color aero-bright_yellow :style 'line)))
     (wgrep-file-face                            (:inherit 'highlight))
     (wgrep-reject-face                          (:foreground aero-bright_red :bold t))

     ;; hydra
     (hydra-face-red (:foreground aero-bright_red :weight 'bold))
     (hydra-face-blue (:foreground aero-bright_blue :weight 'bold))
     (hydra-face-amaranth (:foreground aero-bright_yellow :weight 'bold))
     (hydra-face-pink (:foreground aero-bright_purple :weight 'bold))
     (hydra-face-teal (:foreground aero-bright_aqua :weight 'bold))

     ;; which-function-mode
     (which-func                                 (:foreground aero-faded_blue))

     ;; auto-dim-other-buffers
     (auto-dim-other-buffers-face                (:background aero-bg_inactive)))
    ,@body))

(aero-deftheme
 aero-dark-hard
 "A retro-smooth theme based on Aero"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (aero-dark0_hard      "#000000" "#000000")
  (aero-dark0           "#282828" "#262626")
  (aero-dark0_soft      "#32302f" "#303030")
  (aero-dark1           "#3c3836" "#3a3a3a")
  (aero-dark2           "#504945" "#4e4e4e")
  (aero-dark3           "#665c54" "#626262")
  (aero-dark4           "#7c6f64" "#767676")

  (aero-gray            "#928374" "#8a8a8a")

  (aero-light0_hard     "#ffffc8" "#ffffd7")
  (aero-light0          "#d9d9d9" "#d9d9d9")
  (aero-light1          "#ebdbb2" "#ffdfaf")
  (aero-light2          "#d5c4a1" "#bcbcbc")
  (aero-light3          "#bdae93" "#a8a8a8")
  (aero-light4          "#a89984" "#949494")

  (aero-bright_red      "#fb4933" "#d75f5f")
  (aero-bright_green    "#b8bb26" "#afaf00")
  (aero-bright_yellow   "#fabd2f" "#ffaf00")
  (aero-bright_blue     "#83a598" "#87afaf")
  (aero-bright_purple   "#d3869b" "#d787af")
  (aero-bright_aqua     "#8ec07c" "#87af87")
  (aero-bright_orange   "#fe8019" "#ff8700")

  (aero-faded_red       "#cc241d" "#d75f5f")
  (aero-faded_green     "#98971a" "#afaf00")
  (aero-faded_yellow    "#d79921" "#ffaf00")
  (aero-faded_blue      "#458588" "#87afaf")
  (aero-faded_purple    "#b16286" "#d787af")
  (aero-faded_aqua      "#689d6a" "#87af87")
  (aero-faded_orange    "#d65d0e" "#ff8700")

  (aero-dark_red        "#421E1E" "#5f0000")
  (aero-dark_blue       "#2B3C44" "#000087")
  (aero-dark_aqua       "#36473A" "#005f5f")

  (aero-delimiter-one   "#458588" "#008787")
  (aero-delimiter-two   "#b16286" "#d75f87")
  (aero-delimiter-three "#8ec07c" "#87af87")
  (aero-delimiter-four  "#d65d0e" "#d75f00")
  (aero-white           "#FFFFFF" "#FFFFFF")
  (aero-black           "#000000" "#000000")
  (aero-sienna          "#DD6F48" "#d7875f")
  (aero-lightblue4      "#66999D" "#5fafaf")
  (aero-burlywood4      "#BBAA97" "#afaf87")
  (aero-aquamarine4     "#83A598" "#87af87")
  (aero-turquoise4      "#61ACBB" "#5fafaf")

  (aero-bg aero-dark0_hard)
  (aero-bg_inactive aero-dark0))

 (custom-theme-set-variables 'aero-dark-hard
                             `(ansi-color-names-vector
                               [,aero-dark1
                                ,aero-bright_red
                                ,aero-bright_green
                                ,aero-bright_yellow
                                ,aero-bright_blue
                                ,aero-bright_purple
                                ,aero-bright_aqua
                                ,aero-light1])
           `(pdf-view-midnight-colors '(,aero-light0 . ,aero-bg))))

(show-paren-mode t) ; highlight delimiters
(line-number-mode t) ; only show line number in mode line
(column-number-mode t) ; also show column in mode line
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

(use-package doom-modeline :ensure t
  :config
  (doom-modeline-init)
  (setq doom-modeline-height 20
				doom-modeline-buffer-file-name-style 'buffer-name
				doom-modeline-icon nil)
	(doom-modeline-def-modeline 'aero
		'(bar window-number modals matches buffer-info remote-host buffer-position selection-info)
		'(misc-info persp-name lsp debug fancy-battery minor-modes buffer-encoding major-mode vcs checker))
	(aero/add-hook! 'doom-modeline-mode-hook
						 (doom-modeline-set-modeline 'aero 'default)))

(aero/add-hook!
 'rjsx-mode-hook
 (set-face-attribute 'rjsx-attr nil :inherit font-lock-variable-name-face :slant 'italic)
 (setq emmet-expand-jsx-className? t))

(use-package form-feed
	:load-path aero-packages-directory
	:hook ((prog-mode text-mode) . form-feed-mode))

(use-package dimmer :ensure t
	:defines dimmer-mode
	:config (dimmer-mode))

(use-package hl-todo
  :load-path aero-packages-directory
  :hook (prog-mode . hl-todo-mode))

(provide 'aero-theme)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
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
;;
;;; Commentary:

(require 'subr-x)
(require 'use-package)

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(use-package autothemer :straight t)
(require 'autothemer)

(autothemer-deftheme
 aero-dark "Aero dark theme"

 ((((class color) (min-colors #xFF)))

  (aero-bg             "#121212")
  (aero-fg             "#ffffef")

  (aero-grey0          "#444444")
  (aero-grey1          "#585858")
  (aero-grey2          "#808080")
  (aero-grey3          "#9e9e9e")
  (aero-grey4          "#d0d0d0")
  (aero-grey5          "#c0c0c0")

  (aero-faded-black    "#262626")
  (aero-faded-white    "#dfdfaf")
  (aero-faded-red      "#d7875f")
  (aero-faded-orange   "#ff8700")
  (aero-faded-yellow   "#ffaf00")
  (aero-faded-green    "#afaf00")
  (aero-faded-blue     "#87afaf")
  (aero-faded-cyan     "#5dabab")
  (aero-faded-magenta  "#d787af")

  (aero-bright-black   "#3a3a3a")
  (aero-bright-white   "#ffffd7")
  (aero-bright-red     "#d78787")
  (aero-bright-orange  "#ba7e63")
  (aero-bright-yellow  "#d7af00")
  (aero-bright-green   "#afaf00")
  (aero-bright-blue    "#87afaf")
  (aero-bright-cyan    "#87af87")
  (aero-bright-magenta "#d787af")

  (aero-intense-green  "#5f875f")
  (aero-intense-yellow "#d7d7af")

  (aero-hard-black     "#000000")
  (aero-hard-white     "#ffffff")
  (aero-hard-green     "#005f00")
  (aero-hard-blue      "#00005f")
  (aero-hard-red       "#5f0000"))


 ;; faces (simplified)

 ((default (:background aero-bg :foreground aero-fg :font "Victor Mono" :height 140))
  (cursor (:background aero-fg))
  (mode-line (:background aero-grey0 :foreground aero-grey4 :box nil))
  (mode-line-inactive
   (:background aero-faded-black :foreground aero-grey3 :box nil))
  (hl-line (:background aero-grey0))
  (region (:background aero-grey1))
  (secondary-selection (:background aero-grey0))
  (minibuffer-prompt
   (:background aero-bg :foreground aero-faded-green :bold t))
  (vertical-border (:foreground aero-grey1))
  (window-divider (:foreground aero-grey1))
  (link (:foreground aero-faded-blue :underline t))
  (link-visited (:foreground aero-faded-magenta :underline t))
  (shadow (:foreground aero-grey2))
  (page-break-lines (:foreground aero-grey2))

  ;; Built-in syntax
  (font-lock-builtin-face (:foreground aero-bright-yellow :slant 'italic))
  (font-lock-constant-face (:foreground aero-faded-magenta :slant 'italic))
  (font-lock-comment-face (:foreground aero-grey2 :slant 'italic))
  (font-lock-function-name-face (:foreground aero-bright-orange))
  (font-lock-keyword-face (:foreground aero-faded-red))
  (font-lock-string-face (:foreground aero-bright-green))
  (font-lock-variable-name-face (:foreground aero-faded-white))
  (font-lock-type-face (:foreground aero-bright-magenta))
  (font-lock-warning-face (:foreground aero-bright-red :bold t))
  (font-lock-doc-face (:foreground aero-grey3 :bold t))

  ;; special
  (error (:foreground aero-bright-red :bold t))
  (success (:foreground aero-faded-green :bold t))
  (warning (:foreground aero-bright-yellow :bold t))

  ;; Basic faces
  (trailing-whitespace (:background aero-bright-red))
  (escape-glyph (:foreground aero-faded-blue))
  (header-line
   (:background aero-faded-black :foreground aero-grey4 :box nil :inherit nil))
  (highlight (:background aero-grey2 :foreground aero-fg))
  (homoglyph (:foreground aero-bright-yellow))
  (match (:foreground aero-faded-black :background aero-faded-blue))

  ;; Aero modeline
  (aero/modeline-evil-normal (:foreground aero-bg :background aero-faded-cyan))
  (aero/modeline-evil-insert (:foreground aero-bg :background aero-faded-magenta))
  (aero/modeline-evil-visual (:foreground aero-bg :background aero-faded-green))
  (aero/modeline-evil-replace (:foreground aero-bg :background aero-bright-orange))
  (aero/modeline-evil-emacs (:foreground aero-bg :background aero-bright-red))

  ;; Customize faces
  (widget-field (:background aero-grey1))
  (custom-group-tag (:foreground aero-faded-blue :weight 'bold))
  (custom-variable-tag (:foreground aero-faded-blue :weight 'bold))

  ;; whitespace-mode
  (whitespace-space (:background aero-bg :foreground aero-grey0))
  (whitespace-hspace (:background aero-bg :foreground aero-grey0))
  (whitespace-tab (:background aero-bg :foreground aero-grey0))
  (whitespace-newline (:background aero-bg :foreground aero-grey0))
  (whitespace-trailing (:background aero-grey0 :foreground aero-bright-red))
  (whitespace-line (:background aero-grey0 :foreground aero-bright-red))
  (whitespace-space-before-tab (:background aero-bg :foreground aero-grey0))
  (whitespace-indentation (:background aero-bg :foreground aero-grey0))
  (whitespace-empty (:background aero-bg :foreground aero-grey0))
  (whitespace-space-after-tab (:background aero-bg :foreground aero-grey0))

  ;; Highlight indentation mode
  (highlight-indentation-current-column-face (:background aero-grey1))
  (highlight-indentation-face (:background aero-grey0))

  ;; indent-guide
  (indent-guide-face (:background aero-bg :foreground aero-grey0 :slant 'normal))

  ;; fill column indicator
  (fill-column-indicator (:background aero-bg :foreground aero-grey0))

  ;; smartparens
  (sp-pair-overlay-face (:background aero-grey1))
  ;; Pair tags highlight
  (sp-show-pair-match-face (:background aero-grey1))
  ;; Highlight for bracket without pair
  (sp-show-pair-mismatch-face (:background aero-bright-red))

  ;; elscreen
  ;; Tab bar, not the tabs
  (elscreen-tab-background-face (:background aero-bg :box nil))
  ;; The controls
  (elscreen-tab-control-face
   (:background aero-grey1 :foreground aero-bright-red :underline nil :box nil))
  ;; Current tab
  (elscreen-tab-current-screen-face
   (:background aero-grey2 :foreground aero-faded-black :box nil))
  ;; Inactive tab
  (elscreen-tab-other-screen-face
   (:background aero-grey1 :foreground aero-grey3 :underline nil :box nil))

  ;; ag (The Silver Searcher)
  (ag-hit-face (:foreground aero-faded-blue))
  (ag-match-face (:foreground aero-bright-red))

  ;; diffs
  (diff-changed (:background nil :foreground aero-faded-yellow))
  (diff-added (:background nil :foreground aero-faded-green))
  (diff-refine-added (:background aero-hard-green :foreground aero-bright-green))
  (diff-removed (:background nil :foreground aero-bright-red))
  (diff-refine-removed (:background aero-hard-red :foreground aero-bright-red))
  (diff-indicator-changed (:inherit 'diff-changed))
  (diff-indicator-added (:inherit 'diff-added))
  (diff-indicator-removed (:inherit 'diff-removed))

  ;; smerge
  (smerge-lower (:background aero-hard-green))
  (smerge-upper (:background aero-bright-red))
  (smerge-base (:background aero-hard-blue))
  (smerge-markers (:inherit 'font-lock-comment-face :weight 'bold))

  ;; js2
  (js2-warning (:underline (:color aero-bright-yellow :style 'wave)))
  (js2-error (:underline (:color aero-bright-red :style 'wave)))
  (js2-external-variable (:underline (:color aero-faded-blue :style 'wave)))
  (js2-jsdoc-tag (:background nil :foreground aero-grey2))
  (js2-jsdoc-type (:background nil :foreground aero-grey3))
  (js2-jsdoc-value (:background nil :foreground aero-grey4))
  (js2-function-param (:inherit 'font-lock-variable-name-face :slant 'italic))
  (js2-function-call (:inherit 'font-lock-function-name-face))
  (js2-object-property (:inherit 'font-lock-variable-name-face))
  (js2-instance-member (:background nil :foreground aero-bright-yellow))
  (js2-private-member (:background nil :foreground aero-bright-yellow))
  (js2-private-function-call (:background nil :foreground aero-faded-blue))
  (js2-jsdoc-html-tag-name (:background nil :foreground aero-grey3))
  (js2-jsdoc-html-tag-delimiter (:background nil :foreground aero-grey4))

  ;; rjsx
  (rjsx-attr (:inherit 'js2-object-property :slant 'italic))

  ;; popup
  (popup-face (:underline nil :foreground aero-faded-yellow :background aero-grey0))
  (popup-menu-mouse-face (:underline nil :foreground aero-fg :background aero-faded-green))
  (popup-menu-selection-face (:underline nil :foreground aero-fg :background aero-faded-green))
  (popup-tip-face (:underline nil :foreground aero-grey4 :background aero-grey1))

  ;; counsel
  (counsel-M-x-key (:foreground aero-bright-yellow ))
  (counsel-action (:foreground aero-faded-yellow :underline t))
  (counsel-bookmark-addressbook (:foreground aero-bright-red))
  (counsel-bookmark-directory (:foreground aero-faded-magenta))
  (counsel-bookmark-file (:foreground aero-faded-blue))
  (counsel-bookmark-gnus (:foreground aero-faded-magenta))
  (counsel-bookmark-info (:foreground aero-faded-blue))
  (counsel-bookmark-man (:foreground aero-faded-red))
  (counsel-bookmark-w3m (:foreground aero-bright-yellow))
  (counsel-buffer-directory (:foreground aero-hard-white :background aero-faded-blue))
  (counsel-buffer-not-saved (:foreground aero-bright-red))
  (counsel-buffer-process (:foreground aero-faded-yellow))
  (counsel-buffer-saved-out (:foreground aero-bright-red))
  (counsel-buffer-size (:foreground aero-faded-magenta))
  (counsel-candidate-number (:foreground aero-faded-green))
  (counsel-ff-directory (:foreground aero-faded-magenta))
  (counsel-ff-executable (:foreground aero-faded-blue))
  (counsel-ff-file (:foreground aero-faded-red))
  (counsel-ff-invalid-symlink (:foreground aero-hard-white :background aero-bright-red))
  (counsel-ff-prefix (:foreground aero-hard-black :background aero-bright-yellow))
  (counsel-ff-symlink (:foreground aero-bright-yellow))
  (counsel-grep-cmd-line (:foreground aero-faded-green))
  (counsel-grep-file (:foreground aero-faded-magenta))
  (counsel-grep-finish (:foreground aero-faded-blue))
  (counsel-grep-lineno (:foreground aero-bright-yellow))
  (counsel-grep-match (:foreground aero-bright-yellow))
  (counsel-grep-running (:foreground aero-bright-red))
  (counsel-header (:foreground aero-bright-magenta))
  (counsel-helper (:foreground aero-faded-green))
  (counsel-history-deleted (:foreground aero-hard-black :background aero-bright-red))
  (counsel-history-remote (:foreground aero-bright-red))
  (counsel-lisp-completion-info (:foreground aero-bright-yellow))
  (counsel-lisp-show-completion (:foreground aero-bright-red))
  (counsel-locate-finish (:foreground aero-hard-white :background aero-faded-green))
  (counsel-match (:foreground aero-bright-yellow))
  (counsel-moccur-buffer (:foreground aero-faded-blue :underline t))
  (counsel-prefarg (:foreground aero-faded-blue))
  (counsel-selection (:foreground aero-hard-white :background aero-grey1))
  (counsel-selection-line (:foreground aero-hard-white :background aero-grey1))
  (counsel-separator (:foreground aero-bright-red))
  (counsel-source-header (:foreground aero-grey4))
  (counsel-visible-mark (:foreground aero-hard-black :background aero-grey4))

  ;; avy
  (avy-lead-face (:foreground aero-bright-black :background aero-faded-cyan))
  (avy-lead-face-0 (:foreground aero-faded-black :background aero-bright-blue))
  (avy-lead-face-1 (:foreground aero-faded-white :background aero-grey0))
  (avy-lead-face-2 (:foreground aero-faded-black :background aero-faded-blue))

  ;; hi-lock-mode
  (hi-black-b (:foreground aero-hard-black :weight 'bold))
  (hi-black-hb (:foreground aero-hard-black :weight 'bold :height 1.5))
  (hi-blue (:foreground aero-faded-black :background aero-faded-blue))
  (hi-blue-b (:foreground aero-faded-blue :weight 'bold))
  (hi-green (:foreground aero-faded-black :background aero-faded-green))
  (hi-green-b (:foreground aero-faded-green :weight 'bold))
  (hi-pink (:foreground aero-faded-black :background aero-faded-magenta))
  (hi-red-b (:foreground aero-bright-red :weight 'bold))
  (hi-yellow (:foreground aero-faded-black :background aero-bright-yellow))

  ;; company-mode
  (company-scrollbar-bg (:background aero-grey0))
  (company-scrollbar-fg (:background aero-grey0))
  (company-tooltip (:background aero-grey0))
  (company-tooltip-annotation (:foreground aero-faded-green))
  (company-tooltip-annotation-selection (:inherit 'company-tooltip-annotation))
  (company-tooltip-selection (:foreground aero-faded-magenta :background aero-grey1))
  (company-tooltip-common (:foreground aero-faded-blue :underline t))
  (company-tooltip-common-selection (:foreground aero-faded-blue :underline t))
  (company-preview-common (:foreground aero-fg))
  (company-preview (:background aero-faded-blue))
  (company-preview-search (:background aero-faded-blue))
  (company-template-field (:foreground aero-hard-black :background aero-bright-yellow))
  (company-echo-common (:foreground aero-bright-red))

  ;; tool tips
  (tooltip (:foreground aero-faded-yellow :background aero-grey0))

  ;; term
  (term-color-black (:foreground aero-grey1 :background aero-grey0))
  (term-color-blue (:foreground aero-faded-blue :background aero-faded-blue))
  (term-color-cyan (:foreground aero-faded-blue :background aero-faded-blue))
  (term-color-green (:foreground aero-faded-green :background aero-faded-green))
  (term-color-magenta (:foreground aero-faded-magenta :background aero-faded-magenta))
  (term-color-red (:foreground aero-bright-red :background aero-bright-red))
  (term-color-white (:foreground aero-faded-yellow :background aero-faded-yellow))
  (term-color-yellow (:foreground aero-bright-yellow :background aero-bright-yellow))
  (term-default-fg-color (:foreground aero-fg))
  (term-default-bg-color (:background aero-bg))

  ;; message-mode
  (message-header-to (:inherit 'font-lock-variable-name-face))
  (message-header-cc (:inherit 'font-lock-variable-name-face))
  (message-header-subject (:foreground aero-bright-yellow :weight 'bold))
  (message-header-newsgroups (:foreground aero-bright-yellow :weight 'bold))
  (message-header-other (:inherit 'font-lock-variable-name-face))
  (message-header-name (:inherit 'font-lock-keyword-face))
  (message-header-xheader (:foreground aero-faded-blue))
  (message-separator (:inherit 'font-lock-comment-face))
  (message-cited-text (:inherit 'font-lock-comment-face))
  (message-mml (:foreground aero-faded-green :weight 'bold))

  ;; org-mode
  (org-hide (:foreground aero-faded-black))
  (org-level-1 (:foreground aero-faded-blue))
  (org-level-2 (:foreground aero-bright-yellow))
  (org-level-3 (:foreground aero-faded-magenta))
  (org-level-4 (:foreground aero-bright-red))
  (org-level-5 (:foreground aero-faded-green))
  (org-level-6 (:foreground aero-faded-blue))
  (org-level-7 (:foreground aero-faded-blue))
  (org-level-8 (:foreground aero-bright-yellow))
  (org-special-keyword (:inherit 'font-lock-comment-face))
  (org-drawer (:inherit 'font-lock-function-name-face))
  (org-column (:background aero-faded-black))
  (org-column-title (:background aero-faded-black :underline t :weight 'bold))
  (org-warning (:foreground aero-bright-red :weight 'bold :underline nil :bold t))
  (org-archived (:foreground aero-fg :weight 'bold))
  (org-link (:foreground aero-faded-blue :underline t))
  (org-footnote (:foreground aero-faded-blue :underline t))
  (org-ellipsis (:foreground aero-grey3))
  (org-date (:foreground aero-faded-blue :underline t))
  (org-sexp-date (:foreground aero-faded-blue :underline t))
  (org-tag (:bold t :weight 'bold))
  (org-list-dt (:bold t :weight 'bold))
  (org-todo (:foreground aero-bright-red :weight 'bold :bold t))
  (org-done (:foreground aero-faded-blue :weight 'bold :bold t))
  (org-agenda-done (:foreground aero-faded-blue))
  (org-headline-done (:foreground aero-faded-blue))
  (org-table (:foreground aero-faded-blue))
  (org-block (:background aero-grey0))
  (org-block-begin-line (:background aero-grey0))
  (org-block-end-line (:background aero-grey0))
  (org-formula (:foreground aero-bright-yellow))
  (org-document-title (:foreground aero-faded-blue))
  (org-document-info (:foreground aero-faded-blue))
  (org-agenda-structure (:inherit 'font-lock-comment-face))
  (org-agenda-date-today (:foreground aero-fg :weight 'bold :slant 'italic))
  (org-scheduled (:foreground aero-bright-yellow))
  (org-scheduled-today (:foreground aero-faded-blue))
  (org-scheduled-previously (:foreground aero-bright-red))
  (org-upcoming-deadline (:inherit 'font-lock-keyword-face))
  (org-deadline-announce (:foreground aero-bright-red))
  (org-time-grid (:foreground aero-bright-yellow))
  (org-latex-and-related (:foreground aero-faded-blue))

  ;; org-habit
  (org-habit-clear-face (:background aero-faded-blue))
  (org-habit-clear-future-face (:background aero-faded-blue))
  (org-habit-ready-face (:background aero-faded-green))
  (org-habit-ready-future-face (:background aero-faded-green))
  (org-habit-alert-face (:background aero-bright-yellow))
  (org-habit-alert-future-face (:background aero-bright-yellow))
  (org-habit-overdue-face (:background aero-bright-red))
  (org-habit-overdue-future-face (:background aero-bright-red))

  ;; elfeed
  (elfeed-search-title-face (:foreground aero-grey2  ))
  (elfeed-search-unread-title-face (:foreground aero-fg))
  (elfeed-search-date-face (:inherit 'font-lock-builtin-face :underline t))
  (elfeed-search-feed-face (:inherit 'font-lock-variable-name-face))
  (elfeed-search-tag-face (:inherit 'font-lock-keyword-face))
  (elfeed-search-last-update-face (:inherit 'font-lock-comment-face))
  (elfeed-search-unread-count-face (:inherit 'font-lock-comment-face))
  (elfeed-search-filter-face (:inherit 'font-lock-string-face))

  ;; markdown-mode
  (markdown-header-face-1 (:foreground aero-faded-blue))
  (markdown-header-face-2 (:foreground aero-bright-yellow))
  (markdown-header-face-3 (:foreground aero-faded-magenta))
  (markdown-header-face-4 (:foreground aero-bright-red))
  (markdown-header-face-5 (:foreground aero-faded-green))
  (markdown-header-face-6 (:foreground aero-faded-blue))

  ;; ace-jump-mode
  (ace-jump-face-background (:foreground aero-grey3 :background aero-bg :inverse-video nil))
  (ace-jump-face-foreground (:foreground aero-bright-red :background aero-bg :inverse-video nil))

  ;; ace-window
  (aw-background-face (:forground  aero-faded-yellow :background aero-bg :inverse-video nil))
  (aw-leading-char-face (:foreground aero-bright-red :background aero-bg :height 4.0))

  ;; show-paren
  (show-paren-match (:background aero-grey1 :foreground aero-faded-blue  :weight 'bold))
  (show-paren-mismatch (:background aero-bright-red :foreground aero-grey1 :weight 'bold))

  ;; ivy
  (ivy-current-match (:foreground aero-faded-yellow :weight 'bold :underline t))
  (ivy-minibuffer-match-face-1 (:foreground aero-bright-yellow))
  (ivy-minibuffer-match-face-2 (:foreground aero-bright-yellow))
  (ivy-minibuffer-match-face-3 (:foreground aero-bright-yellow))
  (ivy-minibuffer-match-face-4 (:foreground aero-bright-yellow))

  ;; magit
  (magit-bisect-bad (:foreground aero-bright-red))
  (magit-bisect-good (:foreground aero-faded-green))
  (magit-bisect-skip (:foreground aero-bright-yellow))
  (magit-blame-heading (:foreground aero-fg :background aero-grey1))
  (magit-branch-local (:foreground aero-faded-blue))
  (magit-branch-current (:underline aero-faded-blue :inherit 'magit-branch-local))
  (magit-branch-remote (:foreground aero-faded-magenta))
  (magit-cherry-equivalent (:foreground aero-faded-magenta))
  (magit-cherry-unmatched (:foreground aero-faded-blue))
  (magit-diff-added (:foreground aero-bright-green))
  (magit-diff-added-highlight (:foreground aero-bright-green :inherit 'magit-diff-context-highlight))
  (magit-diff-base (:background aero-bright-yellow :foreground aero-grey4))
  (magit-diff-base-highlight (:background aero-bright-yellow :foreground aero-fg))
  (magit-diff-context (:foreground aero-grey0 :foreground aero-fg))
  (magit-diff-context-highlight (:background aero-faded-black :foreground aero-fg))
  (magit-diff-hunk-heading (:background aero-grey1 :foreground aero-grey4))
  (magit-diff-hunk-heading-highlight (:background aero-grey1 :foreground aero-fg))
  (magit-diff-hunk-heading-selection (:background aero-grey1 :foreground aero-bright-yellow))
  (magit-diff-lines-heading (:background aero-bright-yellow :foreground aero-fg))
  (magit-diff-removed (:foreground aero-bright-red))
  (magit-diff-removed-highlight (:foreground aero-bright-red :inherit 'magit-diff-context-highlight))
  (magit-diffstat-added (:foreground aero-bright-green))
  (magit-diffstat-removed (:foreground aero-bright-red))
  (magit-dimmed (:foreground aero-grey2))
  (magit-hash (:foreground aero-faded-green))
  (magit-log-author (:foreground aero-bright-red))
  (magit-log-date (:foreground aero-faded-blue))
  (magit-log-graph (:foreground aero-grey2))
  (magit-process-ng (:foreground aero-bright-red :weight 'bold))
  (magit-process-ok (:foreground aero-faded-green :weight 'bold))
  (magit-reflog-amend (:foreground aero-faded-magenta))
  (magit-reflog-checkout (:foreground aero-faded-blue))
  (magit-reflog-cherry-pick (:foreground aero-faded-green))
  (magit-reflog-commit (:foreground aero-faded-green))
  (magit-reflog-merge (:foreground aero-faded-green))
  (magit-reflog-other (:foreground aero-faded-blue))
  (magit-reflog-rebase (:foreground aero-faded-magenta))
  (magit-reflog-remote (:foreground aero-faded-blue))
  (magit-reflog-reset (:foreground aero-bright-red))
  (magit-refname (:foreground aero-grey3))
  (magit-section-heading (:foreground aero-bright-yellow :weight 'bold))
  (magit-section-heading-selection (:foreground aero-bright-yellow))
  (magit-section-highlight (:background aero-grey0))
  (magit-sequence-drop (:foreground aero-bright-yellow))
  (magit-sequence-head (:foreground aero-faded-blue))
  (magit-sequence-part (:foreground aero-bright-yellow))
  (magit-sequence-stop (:foreground aero-faded-green))
  (magit-signature-bad (:foreground aero-bright-red :weight 'bold))
  (magit-signature-error (:foreground aero-bright-red))
  (magit-signature-expired (:foreground aero-bright-yellow))
  (magit-signature-good (:foreground aero-faded-green))
  (magit-signature-revoked (:foreground aero-faded-magenta))
  (magit-signature-untrusted (:foreground aero-faded-blue))
  (magit-tag (:foreground aero-bright-yellow))

  ;; flyspell
  (flyspell-duplicate (:underline (:color aero-grey3 :style 'line)))
  (flyspell-incorrect (:underline (:color aero-bright-red :style 'line)))

  ;; langtool
  (langtool-errline (:foreground aero-faded-black :background aero-bright-red))
  (langtool-correction-face (:foreground aero-bright-yellow :weight 'bold))

  ;; latex
  (font-latex-bold-face (:foreground aero-faded-green :bold t))
  (font-latex-italic-face (:foreground aero-faded-green :underline t))
  (font-latex-math-face (:foreground aero-grey4))
  (font-latex-script-char-face (:foreground aero-faded-blue))
  (font-latex-sectioning-5-face (:foreground aero-bright-yellow :bold t))
  (font-latex-sedate-face (:foreground aero-grey3))
  (font-latex-string-face (:foreground aero-bright-yellow))
  (font-latex-verbatim-face (:foreground aero-grey3))
  (font-latex-warning-face (:foreground aero-bright-red :weight 'bold))
  (preview-face (:background aero-grey0))

  ;; mu4e
  (mu4e-header-key-face (:foreground aero-faded-green :weight 'bold ))
  (mu4e-unread-face (:foreground aero-faded-blue :weight 'bold ))
  (mu4e-highlight-face (:foreground aero-faded-green))

  ;; shell script
  (sh-quoted-exec (:foreground aero-faded-magenta))
  (sh-heredoc (:foreground aero-bright-yellow))

  ;; undo-tree
  (undo-tree-visualizer-active-branch-face (:foreground aero-fg))
  (undo-tree-visualizer-current-face (:foreground aero-bright-red))
  (undo-tree-visualizer-default-face (:foreground aero-grey2))
  (undo-tree-visualizer-register-face (:foreground aero-bright-yellow))
  (undo-tree-visualizer-unmodified-face (:foreground aero-faded-blue))

  ;; widget faces
  (widget-button-pressed-face (:foreground aero-bright-red))
  (widget-documentation-face (:foreground aero-faded-green))
  (widget-field (:foreground aero-fg :background aero-grey1))
  (widget-single-line-field (:foreground aero-fg :background aero-grey1))

  ;; eshell
  (eshell-prompt (:foreground aero-faded-blue))
  (eshell-ls-archive-face (:foreground aero-grey4))
  (eshell-ls-backup-face (:foreground aero-grey3))
  (eshell-ls-clutter-face (:foreground aero-bright-yellow :weight 'bold))
  (eshell-ls-directory-face (:foreground aero-bright-yellow))
  (eshell-ls-executable-face (:weight 'bold))
  (eshell-ls-missing-face (:foreground aero-bright-red :bold t))
  (eshell-ls-product-face (:foreground aero-bright-red))
  (eshell-ls-readonly-face (:foreground aero-grey4))
  (eshell-ls-special-face (:foreground aero-bright-yellow :bold t))
  (eshell-ls-symlink-face (:foreground aero-bright-red))
  (eshell-ls-unreadable-face (:foreground aero-bright-red :bold t))

  ;; wgrep
  (wgrep-delete-face (:strike-through aero-bright-red))
  (wgrep-done-face (:foreground aero-faded-blue))
  (wgrep-face (:underline (:color aero-bright-yellow :style 'line)))
  (wgrep-file-face (:inherit 'highlight))
  (wgrep-reject-face (:foreground aero-bright-red :bold t))

  ;; hydra
  (hydra-face-red (:foreground aero-bright-red :weight 'bold))
  (hydra-face-blue (:foreground aero-faded-blue :weight 'bold))
  (hydra-face-amaranth (:foreground aero-bright-yellow :weight 'bold))
  (hydra-face-pink (:foreground aero-faded-magenta :weight 'bold))
  (hydra-face-teal (:foreground aero-faded-blue :weight 'bold))

  ;; which-function-mode
  (which-func (:foreground aero-faded-blue))

  ;; auto-dim-other-buffers
  (auto-dim-other-buffers-face (:background aero-faded-black)))

 ;; Form to evaluate
 (custom-theme-set-variables
  'aero-dark
  `(ansi-color-names-vector
    [,aero-grey0
     ,aero-bright-red
     ,aero-faded-green
     ,aero-bright-yellow
     ,aero-faded-blue
     ,aero-faded-magenta
     ,aero-faded-blue
     ,aero-faded-yellow])
  `(pdf-view-midnight-colors '(,aero-fg . ,aero-bg))))

(defvar aero/modeline--current-window)
(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "flycheck" (errors))

;;; Config

(defgroup aero/modeline nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defface aero/modeline-status-grayed-out
  '((t (:inherit (font-lock-doc-face) :slant italic)))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-info
  '((t (:inherit (font-lock-keyword-face) :slant italic)))
  "Face used for generic status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-success
  '((t (:inherit (success) :slant italic)))
  "Face used for success status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-warning
  '((t (:inherit (warning) :slant italic)))
  "Face for warning status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-error
  '((t (:inherit (error) :slant italic)))
  "Face for error stauts indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-unimportant
  '((t (:inherit (font-lock-doc-face))))
  "Face used for less important mode-line elements."
  :group 'aero/modeline)

(defface aero/modeline-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-evil-normal
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for Normal Evil state message."
  :group 'aero/modeline)

(defface aero/modeline-evil-insert
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for Insert Evil state message."
  :group 'aero/modeline)

(defface aero/modeline-evil-visual
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for Visual Evil state message."
  :group 'aero/modeline)

(defface aero/modeline-evil-replace
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for Replace Evil state message."
  :group 'aero/modeline)

(defface aero/modeline-evil-emacs
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for Emacs Evil state message."
  :group 'aero/modeline)

;;; Helper functions

(defun aero-info-line-format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (concat
     left
     " "
     (propertize  " "
                  'display `((space :align-to (- (+ right right-fringe right-margin) ,(+ reserve 0)))))
     right)))

;; Define a helper function to determine whether or not the current window is active.
(defsubst aero/modeline-is-active ()
  "Return \"t\" if the current window is active, \"nil\" if it is not."
  (eq (selected-window) aero/modeline--current-window))

;;; Update functions

;; Window update function
(defvar-local aero/modeline--current-window (frame-selected-window))
(defun aero/modeline--update-selected-window (&rest _)
  "Update the `aero/modeline--current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq aero/modeline--current-window win)))))

;; Flycheck update function
(defvar-local aero/modeline--flycheck-text nil)
(defun aero/modeline--update-flycheck-segment (&optional status)
  "Update `aero/modeline--flycheck-text' against the reported flycheck STATUS."
  (setq aero/modeline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat " "
                                                 (number-to-string sum)
                                                 " Issues ")
                                         'face (if .error
                                                   'aero/modeline-status-error
                                                 'aero/modeline-status-warning))))
                       (propertize "✔  " 'face 'aero/modeline-status-success)))
          ('running (propertize "Checking  " 'face 'aero/modeline-status-info))
          ('no-checker "")
          ('errored (propertize "✘  " 'face 'aero/modeline-status-error))
          ('interrupted (propertize "⏸  " 'face 'aero/modeline-status-grayed-out)))))

;;; Segments

(defun aero/modeline-segment-evil-state ()
  "Display current evil state. Requires function `evil-mode'."
  (when (require 'evil nil 'noerror)
    (declare-function evil-state-property "evil")
    (defvar evil-state)
    (let* ((state (string-trim (evil-state-property evil-state :tag t)))
           (formed-state (format " %s " state)))
      (cond
       ((string= state "<N>") (propertize formed-state 'face 'aero/modeline-evil-normal))
       ((string= state "<I>") (propertize formed-state 'face 'aero/modeline-evil-insert))
       ((string= state "<V>") (propertize formed-state 'face 'aero/modeline-evil-visual))
       ((string= state "<R>") (propertize formed-state 'face 'aero/modeline-evil-replace))
       ((string= state "<E>") (propertize formed-state 'face 'aero/modeline-evil-emacs))
       (t formed-state)))))

(defun aero/modeline-segment-modified ()
  "Displays a color-coded buffer modification indicator in the mode-line."
  (propertize
   (if (and
        (buffer-modified-p)
        (not (string-match-p "\\*.*\\*" (buffer-name))))
       " ✧ "
     "   ")
   'face 'aero/modeline-modified))

(defun aero/modeline-segment-buffer-name-and-size ()
  "Displays the name and size of the current buffer in the mode-line."
  (concat (propertize "%b (%I)" 'face 'mode-line-buffer-id) "  "))

(defun aero/modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          " "
          (propertize "%p%%"
                      'face
                      (if (aero/modeline-is-active)
                          'aero/modeline-unimportant
                        'mode-line-inactive))
          (when (use-region-p)
            (concat
             "  " (number-to-string (count-lines (point) (mark)))
             ":" (number-to-string (abs (- (point) (mark))))))
          "  "))

(defun aero/modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  aero/modeline--flycheck-text)

(defun aero/modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (list mode-line-process "  ")))

(defun aero/modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize " %m  "
              'face (if (aero/modeline-is-active)
                        'bold
                      'aero/modeline-status-grayed-out)))

(defun aero/modeline-segment-window-number ()
  "Displays the current window number as provided by `winum'."
  (when (require 'winum nil 'noerror)
    (declare-function winum-get-number "winum")
    (concat (format "|%d| " (winum-get-number)))))

;;; Activation function

;; Store the default mode-line format
(defvar aero/modeline--default-mode-line mode-line-format)

(define-minor-mode aero/modeline-mode
  "Toggle aero/modeline on or off."
  :group 'aero/modeline
  :global t
  :lighter nil
  (progn
    ;; Setup flycheck hooks
    (add-hook 'flycheck-status-changed-functions #'aero/modeline--update-flycheck-segment)
    (add-hook 'flycheck-mode-hook #'aero/modeline--update-flycheck-segment)

    ;; Setup window update hooks
    (add-hook 'window-configuration-change-hook #'aero/modeline--update-selected-window)
    (add-hook 'focus-in-hook #'aero/modeline--update-selected-window)
    (advice-add #'handle-switch-frame :after #'aero/modeline--update-selected-window)
    (advice-add #'select-window :after #'aero/modeline--update-selected-window)

    ;; Set the new mode-line-format
    (setq-default mode-line-format
                  '((:eval
                     (aero-info-line-format
                      ;; Left
                      (format-mode-line
                       '((:eval (aero/modeline-segment-evil-state))
                         (:eval (aero/modeline-segment-modified))
                         (:eval (aero/modeline-segment-buffer-name-and-size))
                         (:eval (aero/modeline-segment-position))))

                      ;; Right
                      (format-mode-line
                       '((:eval (aero/modeline-segment-flycheck))
                         (:eval (aero/modeline-segment-process))
                         (:eval (aero/modeline-segment-major-mode))
                         (:eval (aero/modeline-segment-window-number))
                         " "))))))))


;;; headerline

(defgroup aero/headerline nil
  "A simple header-line configuration."
  :group 'header-line)

(defvar aero/headerline--current-window)

(defface aero/headerline-which-function
  '((t (:inherit (which-func) :weight bold)))
  "Face for git branch in header."
  :group 'aero/headerline)

(defsubst aero/headerline-is-active ()
  "Return t if the current window is active, nil if not."
  (eq (selected-window) aero/headerline--current-window))

(defvar-local aero/headerline--current-window (frame-selected-window))
(defun aero/headerline--update-selected-window (&rest _)
  "Update the `aero/headerline--current-window' variable."
  (when (frame-selected-window)
    (let ((win (frame-selected-window)))
      (unless (minibuffer-window-active-p win)
        (setq aero/headerline--current-window win)))))

(defun aero/headerline-segment-which-function ()
  "Display the current function, according to `which-function-mode'."
  (when (require 'which-func nil 'noerror)
    (let ((fun (which-function)))
      (when fun
        (concat
         " –– "
         (propertize (which-function) 'face 'aero/headerline-which-function)
         " ")))))

;; Store the original
(defvar aero/headerline--default-header-line header-line-format)

(define-minor-mode aero/headerline-mode
  "Toggle aero/headerline on or off."
  :group 'aero/headerline
  :global t
  :lighter nil
  (progn
    (add-hook 'window-configuration-change-hook #'aero/headerline--update-selected-window)
    (add-hook 'focus-in-hook #'aero/headerline--update-selected-window)
    (advice-add #'handle-switch-frame :after #'aero/headerline--update-selected-window)
    (advice-add #'select-window :after #'aero/headerline--update-selected-window)

    (setq-default header-line-format
                  '((:eval
                     (aero/info-line-format
                      ;; Left
                      (format-mode-line
                       '((:eval (aero/headerline-segment-which-function))))

                      ;; Right
                      ""))))))

(aero/headerline-mode)

;;; additional tweaks and packages

(when (boundp 'global-prettify-symbols-mode)
  (add-hook
   'prog-mode-hook
   (lambda ()
     (setq prettify-symbols-alist
           (append prettify-symbols-alist
                   '(
                     ;; add all greek
                     ("lambda" . ?λ)

                     ;; mathematics
                     ("and" . ?∧)
                     ("&&" . ?∧)
                     ("or" . ?∨)
                     ("||" . ?∨)

                     ;; relational algebra
                     ("in" . ?∈)
                     ("not in" . ?∉)

                     ;; arrows and similar
                     ("<=" . ?≤)
                     (">=" . ?≥)
                     ("=>" . ?⇒)
                     ("->" . ?→)
                     ("!=" . ?≠)
                     ("===" . ?≡)
                     ("!==" . ?≢)
                     ("<<" . ?≪)
                     (">>" . ?≫))))))
  (global-prettify-symbols-mode t))

(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

(use-package formfeeder
  :load-path "lib/packages/formfeeder/"
  :hook (text-mode . formfeeder-mode)
  :config
  (declare-function global-formfeeder-mode "formfeeder")
  (global-formfeeder-mode 1))

(use-package todo-light
  :load-path "lib/packages/todo-light/"
  :hook ((prog-mode text-mode) . todo-light-mode))

(provide 'aero-theme)

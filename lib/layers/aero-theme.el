;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs
;;
;; Commentary:
;;
;; Code:

(require 'cl-lib)

(use-package autothemer :ensure t)

(autothemer-deftheme
 aero-theme "Aero theme"


 ;; the theme

 ((((class color) (min-colors #xFF)))

  ;; base dark
  (aero-bg              "#151515")
  (aero-dark0           "#212121")
  (aero-dark1           "#3a3a3a")
  (aero-dark2           "#4e4e4e")
  (aero-dark3           "#626262")
  (aero-dark4           "#767676")
  (aero-dark5           "#8a8a8a")

  ;; base light
  (aero-fg              "#d9d9d9")
  (aero-light0          "#ffdfaf")
  (aero-light1          "#949494")
  (aero-light2          "#bcbcbc")
  (aero-light3          "#a8a8a8")

  ;; hard colors
  (aero-black           "#000000")
  (aero-white           "#FFFFFF")
  (aero-hard-green      "#004a00")
  (aero-hard-blue       "#00004a")
  (aero-hard-red        "#4a0000")

  ;; specialty
  (aero-brown           "#DBC075") ; light fade reference
  (aero-blue            "#7eb4b4") ; dark fade reference
  (aero-sienna          "#DB9375")
  (aero-roman-red       "#DB7593")
  (aero-orange          "#EEB958") ; special fade to get that orange
  (aero-green-light     "#CDDB75")
  (aero-green-dark      "#7EB47E")
  (aero-cyan            "#7EA3B4")
  (aero-magenta         "#B47E9E")
  )


 ;; faces (simplified)

 ((default (:background aero-bg :foreground aero-fg :font "Dank Mono"))
  (cursor (:background aero-fg))
  (mode-line (:background aero-dark3 :foreground aero-light2 :box nil))
  (mode-line-inactive
   (:background aero-dark1 :foreground aero-light1 :box nil))
  (hl-line (:background aero-dark1))
  (region (:background aero-dark2))
  (secondary-selection (:background aero-dark1))
  (minibuffer-prompt
   (:background aero-bg :foreground aero-green-light :bold t))
  (vertical-border (:foreground aero-dark2))
  (window-divider (:foreground aero-dark2))
  (link (:foreground aero-blue :underline t))
  (shadow (:foreground aero-dark4))
  (page-break-lines (:foreground aero-dark4))

  ;; Built-in syntax
  (font-lock-builtin-face (:foreground aero-blue))
  (font-lock-constant-face (:foreground aero-magenta))
  (font-lock-comment-face (:foreground aero-dark4 :slant 'italic))
  (font-lock-function-name-face (:foreground aero-green-dark))
  (font-lock-keyword-face (:foreground aero-sienna))
  (font-lock-string-face (:foreground aero-brown))
  (font-lock-variable-name-face (:foreground aero-blue))
  (font-lock-type-face (:foreground aero-green-dark))
  (font-lock-warning-face (:foreground aero-roman-red :bold t))

  ;; special
  (error (:foreground aero-roman-red :bold t))
  (success (:foreground aero-green-light :bold t))
  (warning (:foreground aero-orange :bold t))

  ;; Basic faces
  (trailing-whitespace (:background aero-roman-red))
  (escape-glyph (:foreground aero-blue))
  (header-line
   (:background aero-dark0 :foreground aero-light3 :box nil :inherit nil))
  (highlight (:background aero-dark4 :foreground aero-fg))
  (homoglyph (:foreground aero-orange))
  (match (:foreground aero-dark0 :background aero-blue))

  ;; Customize faces
  (widget-field (:background aero-dark3))
  (custom-group-tag (:foreground aero-blue :weight 'bold))
  (custom-variable-tag (:foreground aero-blue :weight 'bold))

  ;; whitespace-mode
  (whitespace-space (:background aero-bg :foreground aero-dark4))
  (whitespace-hspace (:background aero-bg :foreground aero-dark4))
  (whitespace-tab (:background aero-bg :foreground aero-dark4))
  (whitespace-newline (:background aero-bg :foreground aero-dark4))
  (whitespace-trailing (:background aero-dark1 :foreground aero-roman-red))
  (whitespace-line (:background aero-dark1 :foreground aero-roman-red))
  (whitespace-space-before-tab (:background aero-bg :foreground aero-dark4))
  (whitespace-indentation (:background aero-bg :foreground aero-dark4))
  (whitespace-empty (:background nil :foreground nil))
  (whitespace-space-after-tab (:background aero-bg :foreground aero-dark4))

  ;; Highlight indentation mode
  (highlight-indentation-current-column-face (:background aero-dark2))
  (highlight-indentation-face (:background aero-dark1))

  ;; smartparens
  (sp-pair-overlay-face (:background aero-dark2))
  ;; Pair tags highlight
  (sp-show-pair-match-face (:background aero-dark2))
  ;; Highlight for bracket without pair
  (sp-show-pair-mismatch-face (:background aero-roman-red))

  ;; elscreen
  ;; Tab bar, not the tabs
  (elscreen-tab-background-face (:background aero-bg :box nil))
  ;; The controls
  (elscreen-tab-control-face
   (:background aero-dark2 :foreground aero-roman-red :underline nil :box nil))
  ;; Current tab
  (elscreen-tab-current-screen-face
   (:background aero-dark4 :foreground aero-dark0 :box nil))
  ;; Inactive tab
  (elscreen-tab-other-screen-face
   (:background aero-dark2 :foreground aero-light1 :underline nil :box nil))

  ;; ag (The Silver Searcher)
  (ag-hit-face (:foreground aero-blue))
  (ag-match-face (:foreground aero-roman-red))

  ;; diffs
  (diff-changed (:background nil :foreground aero-light0))
  (diff-added (:background nil :foreground aero-green-light))
  (diff-refine-added (:background aero-hard-green :foreground aero-green-dark))
  (diff-removed (:background nil :foreground aero-roman-red))
  (diff-refine-removed (:background aero-hard-red :foreground aero-roman-red))
  (diff-indicator-changed (:inherit 'diff-changed))
  (diff-indicator-added (:inherit 'diff-added))
  (diff-indicator-removed (:inherit 'diff-removed))

  ;; smerge
  (smerge-lower (:background aero-hard-green))
  (smerge-upper (:background aero-roman-red))
  (smerge-base (:background aero-hard-blue))
  (smerge-markers (:inherit 'font-lock-comment-face :weight 'bold))

  (js2-warning (:underline (:color aero-orange :style 'wave)))
  (js2-error (:underline (:color aero-roman-red :style 'wave)))
  (js2-external-variable (:underline (:color aero-blue :style 'wave)))
  (js2-jsdoc-tag (:background nil :foreground aero-dark5  ))
  (js2-jsdoc-type (:background nil :foreground aero-light1))
  (js2-jsdoc-value (:background nil :foreground aero-light3))
  (js2-function-param (:background nil :foreground aero-blue))
  (js2-function-call (:background nil :foreground aero-blue))
  (js2-instance-member (:background nil :foreground aero-orange))
  (js2-private-member (:background nil :foreground aero-orange))
  (js2-private-function-call (:background nil :foreground aero-blue))
  (js2-jsdoc-html-tag-name (:background nil :foreground aero-light1))
  (js2-jsdoc-html-tag-delimiter (:background nil :foreground aero-light3))

  ;; popup
  (popup-face (:underline nil :foreground aero-light0 :background aero-dark1))
  (popup-menu-mouse-face (:underline nil :foreground aero-fg :background aero-green-light))
  (popup-menu-selection-face (:underline nil :foreground aero-fg :background aero-green-light))
  (popup-tip-face (:underline nil :foreground aero-light2 :background aero-dark2))

  ;; counsel
  (counsel-M-x-key (:foreground aero-orange ))
  (counsel-action (:foreground aero-brown :underline t))
  (counsel-bookmark-addressbook (:foreground aero-roman-red))
  (counsel-bookmark-directory (:foreground aero-magenta))
  (counsel-bookmark-file (:foreground aero-blue))
  (counsel-bookmark-gnus (:foreground aero-magenta))
  (counsel-bookmark-info (:foreground aero-blue))
  (counsel-bookmark-man (:foreground aero-sienna))
  (counsel-bookmark-w3m (:foreground aero-orange))
  (counsel-buffer-directory (:foreground aero-white :background aero-blue))
  (counsel-buffer-not-saved (:foreground aero-roman-red))
  (counsel-buffer-process (:foreground aero-brown))
  (counsel-buffer-saved-out (:foreground aero-roman-red))
  (counsel-buffer-size (:foreground aero-magenta))
  (counsel-candidate-number (:foreground aero-green-light))
  (counsel-ff-directory (:foreground aero-magenta))
  (counsel-ff-executable (:foreground aero-blue))
  (counsel-ff-file (:foreground aero-sienna))
  (counsel-ff-invalid-symlink (:foreground aero-white :background aero-roman-red))
  (counsel-ff-prefix (:foreground aero-black :background aero-orange))
  (counsel-ff-symlink (:foreground aero-orange))
  (counsel-grep-cmd-line (:foreground aero-green-light))
  (counsel-grep-file (:foreground aero-magenta))
  (counsel-grep-finish (:foreground aero-blue))
  (counsel-grep-lineno (:foreground aero-orange))
  (counsel-grep-match (:foreground aero-orange))
  (counsel-grep-running (:foreground aero-roman-red))
  (counsel-header (:foreground aero-green-dark))
  (counsel-helper (:foreground aero-green-dark))
  (counsel-history-deleted (:foreground aero-black :background aero-roman-red))
  (counsel-history-remote (:foreground aero-roman-red))
  (counsel-lisp-completion-info (:foreground aero-orange))
  (counsel-lisp-show-completion (:foreground aero-roman-red))
  (counsel-locate-finish (:foreground aero-white :background aero-green-dark))
  (counsel-match (:foreground aero-orange))
  (counsel-moccur-buffer (:foreground aero-blue :underline t))
  (counsel-prefarg (:foreground aero-blue))
  (counsel-selection (:foreground aero-white :background aero-dark2))
  (counsel-selection-line (:foreground aero-white :background aero-dark2))
  (counsel-separator (:foreground aero-roman-red))
  (counsel-source-header (:foreground aero-light2))
  (counsel-visible-mark (:foreground aero-black :background aero-light3))

  ;;hi-lock-mode
  (hi-black-b (:foreground aero-black :weight 'bold))
  (hi-black-hb (:foreground aero-black :weight 'bold :height 1.5))
  (hi-blue (:foreground aero-dark0 :background aero-blue))
  (hi-blue-b (:foreground aero-blue :weight 'bold))
  (hi-green (:foreground aero-dark0 :background aero-green-light))
  (hi-green-b (:foreground aero-green-light :weight 'bold))
  (hi-pink (:foreground aero-dark0 :background aero-magenta))
  (hi-red-b (:foreground aero-roman-red :weight 'bold))
  (hi-yellow (:foreground aero-dark0 :background aero-orange))

  ;; company-mode
  (company-scrollbar-bg (:background aero-dark1))
  (company-scrollbar-fg (:background aero-dark1))
  (company-tooltip (:background aero-dark1))
  (company-tooltip-annotation (:foreground aero-green-light))
  (company-tooltip-annotation-selection (:inherit 'company-tooltip-annotation))
  (company-tooltip-selection (:foreground aero-magenta :background aero-dark2))
  (company-tooltip-common (:foreground aero-blue :underline t))
  (company-tooltip-common-selection (:foreground aero-blue :underline t))
  (company-preview-common (:foreground aero-fg))
  (company-preview (:background aero-blue))
  (company-preview-search (:background aero-blue))
  (company-template-field (:foreground aero-black :background aero-orange))
  (company-echo-common (:foreground aero-roman-red))

  ;; tool tips
  (tooltip (:foreground aero-light0 :background aero-dark1))

  ;; term
  (term-color-black (:foreground aero-dark2 :background aero-dark1))
  (term-color-blue (:foreground aero-blue :background aero-blue))
  (term-color-cyan (:foreground aero-blue :background aero-blue))
  (term-color-green (:foreground aero-green-light :background aero-green-light))
  (term-color-magenta (:foreground aero-magenta :background aero-magenta))
  (term-color-red (:foreground aero-roman-red :background aero-roman-red))
  (term-color-white (:foreground aero-light0 :background aero-light0))
  (term-color-yellow (:foreground aero-orange :background aero-orange))
  (term-default-fg-color (:foreground aero-fg))
  (term-default-bg-color (:background aero-bg))

  ;; message-mode
  (message-header-to (:inherit 'font-lock-variable-name-face))
  (message-header-cc (:inherit 'font-lock-variable-name-face))
  (message-header-subject (:foreground aero-orange :weight 'bold))
  (message-header-newsgroups (:foreground aero-orange :weight 'bold))
  (message-header-other (:inherit 'font-lock-variable-name-face))
  (message-header-name (:inherit 'font-lock-keyword-face))
  (message-header-xheader (:foreground aero-blue))
  (message-separator (:inherit 'font-lock-comment-face))
  (message-cited-text (:inherit 'font-lock-comment-face))
  (message-mml (:foreground aero-green-light :weight 'bold))

  ;; org-mode
  (org-hide (:foreground aero-dark0))
  (org-level-1 (:foreground aero-blue))
  (org-level-2 (:foreground aero-orange))
  (org-level-3 (:foreground aero-magenta))
  (org-level-4 (:foreground aero-roman-red))
  (org-level-5 (:foreground aero-green-light))
  (org-level-6 (:foreground aero-blue))
  (org-level-7 (:foreground aero-blue))
  (org-level-8 (:foreground aero-orange))
  (org-special-keyword (:inherit 'font-lock-comment-face))
  (org-drawer (:inherit 'font-lock-function-name-face))
  (org-column (:background aero-dark0))
  (org-column-title (:background aero-dark0 :underline t :weight 'bold))
  (org-warning (:foreground aero-roman-red :weight 'bold :underline nil :bold t))
  (org-archived (:foreground aero-fg :weight 'bold))
  (org-link (:foreground aero-blue :underline t))
  (org-footnote (:foreground aero-blue :underline t))
  (org-ellipsis (:foreground aero-light1))
  (org-date (:foreground aero-blue :underline t))
  (org-sexp-date (:foreground aero-blue :underline t))
  (org-tag (:bold t :weight 'bold))
  (org-list-dt (:bold t :weight 'bold))
  (org-todo (:foreground aero-roman-red :weight 'bold :bold t))
  (org-done (:foreground aero-blue :weight 'bold :bold t))
  (org-agenda-done (:foreground aero-blue))
  (org-headline-done (:foreground aero-blue))
  (org-table (:foreground aero-blue))
  (org-block (:background aero-dark1))
  (org-block-begin-line (:background aero-dark1))
  (org-block-end-line (:background aero-dark1))
  (org-formula (:foreground aero-orange))
  (org-document-title (:foreground aero-blue))
  (org-document-info (:foreground aero-blue))
  (org-agenda-structure (:inherit 'font-lock-comment-face))
  (org-agenda-date-today (:foreground aero-fg :weight 'bold :slant 'italic))
  (org-scheduled (:foreground aero-orange))
  (org-scheduled-today (:foreground aero-blue))
  (org-scheduled-previously (:foreground aero-roman-red))
  (org-upcoming-deadline (:inherit 'font-lock-keyword-face))
  (org-deadline-announce (:foreground aero-roman-red))
  (org-time-grid (:foreground aero-orange))
  (org-latex-and-related (:foreground aero-blue))

  ;; org-habit
  (org-habit-clear-face (:background aero-blue))
  (org-habit-clear-future-face (:background aero-blue))
  (org-habit-ready-face (:background aero-green-light))
  (org-habit-ready-future-face (:background aero-green-light))
  (org-habit-alert-face (:background aero-orange))
  (org-habit-alert-future-face (:background aero-orange))
  (org-habit-overdue-face (:background aero-roman-red))
  (org-habit-overdue-future-face (:background aero-roman-red))

  ;; elfeed
  (elfeed-search-title-face (:foreground aero-dark5  ))
  (elfeed-search-unread-title-face (:foreground aero-fg))
  (elfeed-search-date-face (:inherit 'font-lock-builtin-face :underline t))
  (elfeed-search-feed-face (:inherit 'font-lock-variable-name-face))
  (elfeed-search-tag-face (:inherit 'font-lock-keyword-face))
  (elfeed-search-last-update-face (:inherit 'font-lock-comment-face))
  (elfeed-search-unread-count-face (:inherit 'font-lock-comment-face))
  (elfeed-search-filter-face (:inherit 'font-lock-string-face))

  ;; markdown-mode
  (markdown-header-face-1 (:foreground aero-blue))
  (markdown-header-face-2 (:foreground aero-orange))
  (markdown-header-face-3 (:foreground aero-magenta))
  (markdown-header-face-4 (:foreground aero-roman-red))
  (markdown-header-face-5 (:foreground aero-green-light))
  (markdown-header-face-6 (:foreground aero-blue))

  ;; ace-jump-mode
  (ace-jump-face-background (:foreground aero-light1 :background aero-bg :inverse-video nil))
  (ace-jump-face-foreground (:foreground aero-roman-red :background aero-bg :inverse-video nil))

  ;; ace-window
  (aw-background-face (:forground  aero-light0 :background aero-bg :inverse-video nil))
  (aw-leading-char-face (:foreground aero-roman-red :background aero-bg :height 4.0))

  ;; show-paren
  (show-paren-match (:background aero-dark3 :foreground aero-blue  :weight 'bold))
  (show-paren-mismatch (:background aero-roman-red :foreground aero-dark3 :weight 'bold))

  ;; ivy
  (ivy-current-match (:foreground aero-brown :weight 'bold :underline t))
  (ivy-minibuffer-match-face-1 (:foreground aero-orange))
  (ivy-minibuffer-match-face-2 (:foreground aero-orange))
  (ivy-minibuffer-match-face-3 (:foreground aero-orange))
  (ivy-minibuffer-match-face-4 (:foreground aero-orange))

  ;; magit
  (magit-bisect-bad (:foreground aero-roman-red))
  (magit-bisect-good (:foreground aero-green-light))
  (magit-bisect-skip (:foreground aero-orange))
  (magit-blame-heading (:foreground aero-fg :background aero-dark2))
  (magit-branch-local (:foreground aero-blue))
  (magit-branch-current (:underline aero-blue :inherit 'magit-branch-local))
  (magit-branch-remote (:foreground aero-green-light))
  (magit-cherry-equivalent (:foreground aero-magenta))
  (magit-cherry-unmatched (:foreground aero-blue))
  (magit-diff-added (:foreground aero-green-light))
  (magit-diff-added-highlight (:foreground aero-green-light :inherit 'magit-diff-context-highlight))
  (magit-diff-base (:background aero-orange :foreground aero-light2))
  (magit-diff-base-highlight (:background aero-orange :foreground aero-fg))
  (magit-diff-context (:foreground aero-dark1  :foreground aero-light0))
  (magit-diff-context-highlight (:background aero-dark0 :foreground aero-fg))
  (magit-diff-hunk-heading (:background aero-dark3 :foreground aero-light2))
  (magit-diff-hunk-heading-highlight (:background aero-dark2 :foreground aero-fg))
  (magit-diff-hunk-heading-selection (:background aero-dark2 :foreground aero-orange))
  (magit-diff-lines-heading (:background aero-orange :foreground aero-fg))
  (magit-diff-removed (:foreground aero-roman-red))
  (magit-diff-removed-highlight (:foreground aero-roman-red :inherit 'magit-diff-context-highlight))
  (magit-diffstat-added (:foreground aero-green-light))
  (magit-diffstat-removed (:foreground aero-roman-red))
  (magit-dimmed (:foreground aero-dark4))
  (magit-hash (:foreground aero-blue))
  (magit-log-author (:foreground aero-roman-red))
  (magit-log-date (:foreground aero-blue))
  (magit-log-graph (:foreground aero-dark4))
  (magit-process-ng (:foreground aero-roman-red :weight 'bold))
  (magit-process-ok (:foreground aero-green-light :weight 'bold))
  (magit-reflog-amend (:foreground aero-magenta))
  (magit-reflog-checkout (:foreground aero-blue))
  (magit-reflog-cherry-pick (:foreground aero-green-light))
  (magit-reflog-commit (:foreground aero-green-light))
  (magit-reflog-merge (:foreground aero-green-light))
  (magit-reflog-other (:foreground aero-blue))
  (magit-reflog-rebase (:foreground aero-magenta))
  (magit-reflog-remote (:foreground aero-blue))
  (magit-reflog-reset (:foreground aero-roman-red))
  (magit-refname (:foreground aero-light1))
  (magit-section-heading (:foreground aero-orange :weight 'bold))
  (magit-section-heading-selection (:foreground aero-orange))
  (magit-section-highlight (:background aero-dark1))
  (magit-sequence-drop (:foreground aero-orange))
  (magit-sequence-head (:foreground aero-blue))
  (magit-sequence-part (:foreground aero-orange))
  (magit-sequence-stop (:foreground aero-green-light))
  (magit-signature-bad (:foreground aero-roman-red :weight 'bold))
  (magit-signature-error (:foreground aero-roman-red))
  (magit-signature-expired (:foreground aero-orange))
  (magit-signature-good (:foreground aero-green-light))
  (magit-signature-revoked (:foreground aero-magenta))
  (magit-signature-untrusted (:foreground aero-blue))
  (magit-tag (:foreground aero-orange))

  ;; flyspell
  (flyspell-duplicate (:underline (:color aero-light1 :style 'line)))
  (flyspell-incorrect (:underline (:color aero-roman-red :style 'line)))

  ;; langtool
  (langtool-errline (:foreground aero-dark0 :background aero-roman-red))
  (langtool-correction-face (:foreground aero-orange :weight 'bold))

  ;; latex
  (font-latex-bold-face (:foreground aero-green-light :bold t))
  (font-latex-italic-face (:foreground aero-green-light :underline t))
  (font-latex-math-face (:foreground aero-light3))
  (font-latex-script-char-face (:foreground aero-blue))
  (font-latex-sectioning-5-face (:foreground aero-orange :bold t))
  (font-latex-sedate-face (:foreground aero-light1))
  (font-latex-string-face (:foreground aero-orange))
  (font-latex-verbatim-face (:foreground aero-light1))
  (font-latex-warning-face (:foreground aero-roman-red :weight 'bold))
  (preview-face (:background aero-dark1))

  ;; mu4e
  (mu4e-header-key-face (:foreground aero-green-light :weight 'bold ))
  (mu4e-unread-face (:foreground aero-blue :weight 'bold ))
  (mu4e-highlight-face (:foreground aero-green-light))

  ;; shell script
  (sh-quoted-exec (:foreground aero-magenta))
  (sh-heredoc (:foreground aero-orange))

  ;; undo-tree
  (undo-tree-visualizer-active-branch-face (:foreground aero-fg))
  (undo-tree-visualizer-current-face (:foreground aero-roman-red))
  (undo-tree-visualizer-default-face (:foreground aero-dark4))
  (undo-tree-visualizer-register-face (:foreground aero-orange))
  (undo-tree-visualizer-unmodified-face (:foreground aero-blue))

  ;; widget faces
  (widget-button-pressed-face (:foreground aero-roman-red))
  (widget-documentation-face (:foreground aero-green-light))
  (widget-field (:foreground aero-fg :background aero-dark2))
  (widget-single-line-field (:foreground aero-fg :background aero-dark2))

  ;; eshell
  (eshell-prompt (:foreground aero-blue))
  (eshell-ls-archive-face (:foreground aero-light3))
  (eshell-ls-backup-face (:foreground aero-light1))
  (eshell-ls-clutter-face (:foreground aero-orange :weight 'bold))
  (eshell-ls-directory-face (:foreground aero-orange))
  (eshell-ls-executable-face (:weight 'bold))
  (eshell-ls-missing-face (:foreground aero-roman-red :bold t))
  (eshell-ls-product-face (:foreground aero-roman-red))
  (eshell-ls-readonly-face (:foreground aero-light2))
  (eshell-ls-special-face (:foreground aero-orange :bold t))
  (eshell-ls-symlink-face (:foreground aero-roman-red))
  (eshell-ls-unreadable-face (:foreground aero-roman-red :bold t))

  ;; wgrep
  (wgrep-delete-face (:strike-through aero-roman-red))
  (wgrep-done-face (:foreground aero-blue))
  (wgrep-face (:underline (:color aero-orange :style 'line)))
  (wgrep-file-face (:inherit 'highlight))
  (wgrep-reject-face (:foreground aero-roman-red :bold t))

  ;; rjsx
  (rjsx-attr (:foreground aero-light2 :slant 'italic))

  ;; hydra
  (hydra-face-red (:foreground aero-roman-red :weight 'bold))
  (hydra-face-blue (:foreground aero-blue :weight 'bold))
  (hydra-face-amaranth (:foreground aero-orange :weight 'bold))
  (hydra-face-pink (:foreground aero-magenta :weight 'bold))
  (hydra-face-teal (:foreground aero-blue :weight 'bold))

  ;; which-function-mode
  (which-func (:foreground aero-blue))

  ;; auto-dim-other-buffers
  (auto-dim-other-buffers-face (:background aero-dark0)))


 ;; Form to evaluate
 (custom-theme-set-variables
  'aero-theme
  `(ansi-color-names-vector
    [,aero-dark1
     ,aero-roman-red
     ,aero-green-light
     ,aero-orange
     ,aero-blue
     ,aero-magenta
     ,aero-blue
     ,aero-light0])
  `(pdf-view-midnight-colors '(,aero-fg . ,aero-bg))))

;;; mode line

(defvar aero/modeline--current-window)
(defvar flycheck-current-errors)
(declare-function flycheck-count-errors "flycheck" (errors))

;; Config

(defgroup aero/modeline nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom aero/modeline-show-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'aero/modeline
  :type 'boolean)

(defface aero/modeline-status-grayed-out
  '((t (:inherit (font-lock-doc-face))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'aero/modeline)

(defface aero/modeline-status-error
  '((t (:inherit (error))))
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

;; Helper functions

(defun aero/modeline-format (left right)
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
                             (propertize (concat "Issues: "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'aero/modeline-status-error
                                                 'aero/modeline-status-warning))))
                       (propertize "✔  " 'face 'aero/modeline-status-success)))
          ('running (propertize "Checking  " 'face 'aero/modeline-status-info))
          ('no-checker "")
          ('errored (propertize "Error  " 'face 'aero/modeline-status-error))
          ('interrupted (propertize "Paused  " 'face 'aero/modeline-status-grayed-out)))))

;; Segments

(defun aero/modeline-segment-modified ()
  "Displays a color-coded buffer modification indicator in the mode-line."
  (propertize
   (if (and
        (buffer-modified-p)
        (not (string-match-p "\\*.*\\*" (buffer-name))))
       " ✧ "
     "   ")
   'face 'aero/modeline-modified))

(defun aero/modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (concat (propertize "%b" 'face 'mode-line-buffer-id) "  "))

(defun aero/modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          (when aero/modeline-show-point
            (concat ":"
                    (propertize (format "%d" (point)) 'face (if (aero/modeline-is-active)
                                                                'aero/modeline-unimportant
                                                              'mode-line-inactive))))
          " "
          (propertize "%p%%" 'face (if (aero/modeline-is-active)
                                       'aero/modeline-unimportant
                                     'mode-line-inactive))
          "  "))

(defun aero/modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  "
              'face (if (aero/modeline-is-active)
                        'bold
                      'aero/modeline-status-grayed-out)))

(defun aero/modeline-segment-global-mode-string ()
  "Displays the current value of `global-mode-string' in the mode-line."
  (when (not (string= (mapconcat 'concat (mapcar 'eval global-mode-string) "") ""))
    (propertize "%M  "
                'face 'aero/modeline-status-grayed-out)))

(defun aero/modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  aero/modeline--flycheck-text)

(defun aero/modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (list mode-line-process "  ")))

(defun aero/modeline-segment-evil-state ()
  "Display current evil state. Requires `evil-mode'."
  (format " %s" (string-trim (evil-state-property evil-state :tag t))))

;; Activation function

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
                     (aero/modeline-format
                      ;; Left
                      (format-mode-line
                       '((:eval (aero/modeline-segment-evil-state))
                         (:eval (aero/modeline-segment-modified))
                         (:eval (aero/modeline-segment-buffer-name))
                         (:eval (aero/modeline-segment-position))))

                      ;; Right
                      (format-mode-line
                       '((:eval (aero/modeline-segment-major-mode))
                         (:eval (aero/modeline-segment-global-mode-string))
                         (:eval (aero/modeline-segment-flycheck))
                         (:eval (aero/modeline-segment-process))
                         " "))))))))

;; Do it
(aero/modeline-mode)


;;; additional tweaks and packages

(when (boundp 'global-prettify-symbols-mode)
  (add-hook
   'prog-mode-hook
   (lambda ()
     (setq prettify-symbols-alist
           (append prettify-symbols-alist
                   '(
                     ;; add all greek
                     ("Alpha" . ?Α)
                     ("Beta" . ?Β)
                     ("Gamma" . ?Γ)
                     ("Delta" . ?Δ)
                     ("Epsilon" . ?Ε)
                     ("Zeta" . ?Ζ)
                     ("Eta" . ?Η)
                     ("Theta" . ?Θ)
                     ("Iota" . ?Ι)
                     ("Kappa" . ?Κ)
                     ("Lambda" . ?Λ)
                     ("Mu" . ?Μ)
                     ("Nu" . ?Ν)
                     ("Xi" . ?Ξ)
                     ("Omicron" . ?Ο)
                     ("Pi" . ?Π)
                     ("Rho" . ?Ρ)
                     ("Sigma" . ?Σ)
                     ("Tau" . ?Τ)
                     ("Upsilon" . ?Υ)
                     ("Phi" . ?Φ)
                     ("Chi" . ?Χ)
                     ("Psi" . ?Ψ)
                     ("Omega" . ?Ω)
                     ("alpha" . ?α)
                     ("beta" . ?β)
                     ("gamma" . ?γ)
                     ("delta" . ?δ)
                     ("epsilon" . ?ε)
                     ("zeta" . ?ζ)
                     ("eta" . ?η)
                     ("theta" . ?θ)
                     ("iota" . ?ι)
                     ("kappa" . ?κ)
                     ("lambda" . ?λ)
                     ("mu" . ?μ)
                     ("nu" . ?ν)
                     ("xi" . ?ξ)
                     ("omicron" . ?ο)
                     ("pi" . ?π)
                     ("rho" . ?ρ)
                     ("sigma" . ?σ)
                     ("sigma-final" . ?ς)
                     ("tau" . ?τ)
                     ("upsilon" . ?υ)
                     ("phi" . ?φ)
                     ("chi" . ?χ)
                     ("psi" . ?ψ)
                     ("omega" . ?ω)

                     ;; mathematics
                     ("and" . ?∧)
                     ("&&" . ?∧)
                     ("or" . ?∨)
                     ("||" . ?∨)
                     ("nil" . ?∅)
                     ("null" . ?∅)

                     ;; arrows and similar
                     ("<=" . ?≤)
                     (">=" . ?≥)
                     ("<-" . ?←)
                     ("->" . ?→)
                     ("!=" . ?≠)
                     ("===" . ?≡)
                     ("!==" . ?≢)

                     ;; prog
                     ("fn" . ?ƒ)
                     ("defun" . ?ƒ)
                     ("proc" . ?ƒ)
                     ("function" . ?ƒ)

                     ;; other
                     ("(c)" . ?©)
                     ("(C)" . ?©))))))
  (global-prettify-symbols-mode t))

(byte-code "\300\301!\210\302\301!\210\303\301!\207" [show-paren-mode
                                                      t line-number-mode column-number-mode] 2)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(add-hook 'fundamental-mode-hook 'turn-on-visual-line-mode)

(use-package formfeeder
  :load-path "lib/packages/formfeeder/"
  :config
  (global-formfeeder-mode 1))

(use-package todo-light
  :load-path "lib/packages/todo-light/"
  :hook ((prog-mode text-mode) . todo-light-mode))

(provide 'aero-theme)

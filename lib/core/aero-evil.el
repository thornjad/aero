;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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
;;
;;; Code:

(package! evil :auto
  :init
  ;; Need to be in init because of something in the way the "want" variables are used
  (setq evil-want-keybinding nil ; handled by evil-collection
        ;; to change undo-system without restart, use SPC-: `evil-set-undo-system'
        evil-undo-system 'undo-tree
        evil-want-fine-undo t
        evil-want-C-i-jump nil
        evil-want-C-u-scroll t)

  :config

  ;; We use SPC as the leader key so it shouldn't do anything when in motion
  (define-key evil-motion-state-map " " nil)

  ;; default states
  (setq evil-default-state 'normal)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'message-mode 'motion)

  ;; Make movement keys work like they should by remapping next to next-visual, etc.
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  ;; Ensure horizontal movement doesn't cross to the next/previous line
  (setq-default evil-cross-lines nil)

  ;; Undo in region
  (define-key evil-visual-state-map (kbd "u") 'undo)

  ;; By default, these two operate on half pages, but I prefer the smaller jump
  (defun aero/scroll-quarter-page-down ()
    (interactive)
    (evil-scroll-down (/ (window-body-height) 4)))
  (defun aero/scroll-quarter-page ()
    (interactive)
    (evil-scroll-up (/ (window-body-height) 4)))
  (evil-define-key nil global-map (kbd "C-u") #'aero/scroll-quarter-page-up)
  (evil-define-key nil global-map (kbd "C-d") #'aero/scroll-quarter-page-down)

  ;; Define vig and vag, etc. to look for all paren types
  (defun aero/evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
    (let* ((parens '("()" "[]" "{}" "<>"))
           range
           found-range)
      (dolist (p parens)
        (condition-case nil
            (setq range (evil-select-paren (aref p 0) (aref p 1) beg end type count inclusive))
          (error nil))
        (when range
          (cond
           (found-range
            (when (< (- (nth 1 range) (nth 0 range))
                     (- (nth 1 found-range) (nth 0 found-range)))
              (setf (nth 0 found-range) (nth 0 range))
              (setf (nth 1 found-range) (nth 1 range))))
           (t
            (setq found-range range)))))
      found-range))
  (evil-define-text-object aero/evil-a-paren (count &optional beg end type)
    "Select a paren."
    :extend-selection t
    (aero/evil-paren-range count beg end type t))
  (evil-define-text-object aero/evil-inner-paren (count &optional beg end type)
    "Select 'inner' paren."
    :extend-selection nil
    (aero/evil-paren-range count beg end type nil))
  (define-key evil-inner-text-objects-map "g" #'aero/evil-inner-paren)
  (define-key evil-outer-text-objects-map "g" #'aero/evil-a-paren)

  ;; Very useful, in visual mode, use < and > to indent/unindent the line(s)
  (defun aero/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (defun aero/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  (evil-define-key 'visual global-map (kbd ">") 'aero/evil-shift-right)
  (evil-define-key 'visual global-map (kbd "<") 'aero/evil-shift-left)

  ;; :q should kill the current buffer rather than quitting Emacs entirely
  (evil-ex-define-cmd "q" 'kill-this-buffer)

  ;; Unless I'm mistaken, there's no Evil backward equivalent to "e", so we'll invent it.
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-e") #'evil-backward-word-end)
  (evil-define-key '(normal visual motion) global-map
    (kbd "C-M-e") #'evil-backward-WORD-end)

  ;; Useful for pasting into the minibuffer where Evil modes usually don't properly function
  (evil-define-key '(insert) global-map
    (kbd "C-y") #'evil-paste-after)
  (evil-define-key '(insert) global-map
    (kbd "C-S-y") #'evil-paste-before)

  ;; Run macro in register q
  (evil-define-key 'normal 'global "Q" "@q")
  (evil-define-key 'visual 'global
    ;; run macro in register q on region
    "Q" (kbd ":norm @q RET")
    ;; repeat on region
    "." (kbd ":norm . RET"))

  ;; activate
  (evil-mode +1))

;; Provides defaults for many modes which evil proper overlooks
(package! evil-collection :auto :after evil :config (evil-collection-init))

(package! evil-matchit :auto :defer 5
  ;; allows % to jump matching tags
  :after evil
  :defines global-evil-matchit-mode
  :config (global-evil-matchit-mode 1))

(provide 'aero-evil)
;;; aero-evil.el ends here

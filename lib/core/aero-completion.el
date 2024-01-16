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

(package! counsel :auto
  :after general
  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
                "\\|\\.aux\\'"))

  (defun aero/counsel-unicode-char-after ()
    "Like `counsel-unicode-char', but insert after point"
    (interactive)
    (save-excursion
      (right-char)
      (counsel-unicode-char)))

  (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
        counsel-git-cmd "rg --files"
        counsel-rg-base-command "rg --with-filename --smart-case --no-heading --line-number --color never %s")

  (aero-leader-def
    "SPC" 'counsel-M-x
    "ff" 'counsel-find-file
    "fl" 'counsel-locate
    "fr" 'counsel-recentf
    "?" 'counsel-rg
    "gg" '(counsel-git-grep :wk "git grep")
    "gff" '(counsel-git :wk "find git file")
    "qu" '(aero/counsel-unicode-char-after :wk "unicode char")
    "qU" 'counsel-unicode-char))

(package! amx :auto
  ;; Enhances counsel-M-x by showing recently used commands and keyboard shortcuts
  :after (counsel)
  :config (amx-mode 1))

(package! recentf :builtin
  :defer 1
  ;; Doesn't seem like indent activates properly for me without this intervention. Here we move it
  ;; to a known cache file and set up an auto-save every 5 minutes.
  :defines (recentf-mode)
  :config
  (setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
        recentf-max-saved-items 500)
  (recentf-mode 1)
  (defun aero/recentf-save-list-quiet ()
    "Wrapper for `recentf-save-list' with no message."
    (let ((inhibit-message t))
      (recentf-save-list)))
  ;; Would be a great place for `aero/advice-no-message' but there's no need to hide messaging if
  ;; recentf saves for some other reason. Here, we run it regularly so we don't care about the
  ;; constant messaging.
  (run-at-time 60 (* 5 60) #'aero/recentf-save-list-quiet))

(package! ivy :auto
  ;; Despite a general trend in the (loud part of the) community to move away from ivy, I'm still a
  ;; big fan. It's fast, its fully-featured and it has many useful integrations.
  :after general
  :config
  ;; Note: flx is a popular fuzzy matching package, but it refuses to prioritize exact matches,
  ;; which gets too annoying to use.

  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil ; don't pre-populate our search
        ivy-use-virtual-buffers t ; add recentf to `ivy-switch-buffer'
        ivy-virtual-abbreviate 'full
        ;; Counting results isn't really that useful and slows down searching large projects
        ;; significantly, so just forget it.
        ivy-count-format ""
        ivy-wrap t ; wrap top to bottom
        ivy-height 12
        ivy-fixed-height-minibuffer t ; better visual consistency
        ivy-on-del-error-function #'ignore ; don't punish me when I accidentally delete search
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (aero-leader-def
    "bb" 'ivy-switch-buffer
    "R" 'ivy-resume))

(package! ivy-rich :auto
  ;; Adds information about various results in the ivy buffer
  :after (counsel ivy)
  :defines (ivy-rich-path-style)
  :functions (ivy-rich-mode)
  :config
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1)

  (defun aero/ivy-rich--switch-buffer-directory (orig-fun &rest args)
    "Advice to help ivy-rich see that files are not directories."
    (cl-letf (((symbol-function 'directory-file-name) #'file-name-directory))
      (apply orig-fun args)))
  (advice-add 'ivy-rich--switch-buffer-directory :around #'aero/ivy-rich--switch-buffer-directory))

(package! ivy-posframe :auto
  ;; ivy-posframe moves all ivy functions to a floating posframe in the centerish of the screen,
  ;; much like many other editors.
  ;;
  ;; I continually turn this on and off, and cannot decide if I like it. Thus it is not
  ;; auto-activated, you must call M-x ivy-posframe-mode
  :after (ivy)
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-display-function-fallback) ; don't cover search results
          (counsel-rg . ivy-display-function-fallback)
          (flyspell . ivy-display-function-fallback)
          (flyspell-correct-next . ivy-display-function-fallback)
          (flyspell-correct-previous . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point) ; could cover point
          (t . ivy-posframe-display)))

  ;; Fix atrocious width jumping
  (defun aero/ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (setq ivy-posframe-size-function 'aero/ivy-posframe-get-size))

(package! all-the-icons :auto
  ;; Add support for icon insertion, and use as a lib in other packages
  :after (general)
  :when (display-graphic-p)
  :config (aero-leader-def "qi" 'all-the-icons-insert))

(package! all-the-icons-ivy-rich (:host github :repo "seagle0128/all-the-icons-ivy-rich")
  ;; Add icons to ivy via ivy-rich
  :after (all-the-icons ivy-rich)
  :functions (all-the-icons-ivy-rich-mode)
  :config (all-the-icons-ivy-rich-mode +1))

(package! swiper :auto
  ;; Search utility
  :after (general counsel)
  :commands (swiper counsel-grep-or-swiper swiper-thing-at-point)
  :init
  (aero-leader-def
    "/" '(counsel-grep-or-swiper :wk "search")
    "?" '(swiper-thing-at-point :wk "search thing at point")
    "b/" '(swiper-all :wk "search all buffers")
    "b?" '(swiper-all-thing-at-point :wk "search thing at point in all buffers"))
  :config
  (setq swiper-action-recenter t))

(package! avy :auto
  ;; visual navigation utility
  :init
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "jl" '(avy-goto-line :wk "jump to line")
   "jc" '(avy-goto-char :wk "jump to char")
   "jj" '(avy-goto-char :wk "jump to char")
   "jw" '(avy-goto-word-1 :wk "jump to word")))

(package! ace-link (:host github :repo "abo-abo/ace-link")
  ;; jump to search results in eww
  :after (avy eww)
  :functions (ace-link-setup-default)
  :config (ace-link-setup-default))

(provide 'aero-completion)
;;; aero-completion.el ends here

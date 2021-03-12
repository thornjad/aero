;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
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
;; This is simply a heap of miscellaneous, low-config packages.

(require 'aero-prelude)

;; Code:

(use-package esup :straight t
  :commands esup)

(use-package rainbow-mode :straight t
  :commands rainbow-mode)

(use-package hackernews
  ;; local
  :commands hackernews)

(use-package pdf-tools
  :when window-system
  :init (pdf-tools-install))


(use-package sx
  :after (general)
  :commands (sx-search)
  :defines (sx-default-site)
  :functions (sx-display)
  :init
  (aero-leader-def
    "wx" 'sx-search)
  :config
  (setq sx-default-site "stackoverflow")
  (evil-define-key 'normal sx-question-list-mode-map (kbd "RET") #'sx-display))


;;; games

(use-package 2048-game :straight t
  :commands 2048-game
  :config
  (general-define-key
   :modes '(normal emacs)
   :keymaps '2048-mode-map
   "h" '2048-left
   "j" '2048-down
   "k" '2048-up
   "l" '2048-right
   "r" '2048-random-move
   (kbd "<up>") '2048-up
   (kbd "<left>") '2048-left
   (kbd "<right>") '2048-right
   (kbd "<down>") '2048-down
   "q" 'quit-window))

(use-package landmark :straight t :defer t)

(provide 'aero-heap)

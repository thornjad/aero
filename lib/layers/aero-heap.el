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
;; This is simply a heap of miscellaneous, low-config packages.
;;
;; Code:

(use-package whitespace-cleanup-mode
	:ensure t
	:config (global-whitespace-cleanup-mode))

(use-package esup :ensure t
  :commands esup)

(use-package rainbow-mode :ensure t
  :commands rainbow-mode)

(use-package hackernews
  ;; local
  :commands hackernews)

;;; games

(use-package 2048-game :ensure t
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

(use-package landmark :ensure t :defer t)

(provide 'aero-heap)

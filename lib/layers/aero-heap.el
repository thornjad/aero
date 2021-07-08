;;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2021 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs
;;
;;; Commentary:
;;
;; This is simply a heap of miscellaneous, low-config packages.

;;; Code:

(require 'aero-prelude)

(use-package esup :straight t
  :commands esup)

(use-package rainbow-mode :straight t
  :commands rainbow-mode)

(use-package hackernews
  ;; local
  :commands hackernews)

(use-package twittering-mode
  :commands (twittering-mode twit)
  :after (general sauron)
  :init
  (defun aero/turn-on-twittering-notifications ()
    (setq sauron-prio-twittering-mention 4))

  (defun aero/start-or-jump-to-twitter ()
    "If twittering-mode is already active, jump to it, otherwise start it."
    (interactive)
    (if (get-buffer "(:home+@)")
        (switch-to-buffer "(:home+@)")
      ;; disable twitter notifications for ~10 seconds
      (setq sauron-prio-twittering-mention 2)
      (twittering-mode)
      (run-at-time "10 sec" nil #'aero/turn-on-twittering-notifications)))

  :config
  (setq twittering-icon-mode t
        twittering-use-master-password t
        twittering-username "jmthorntonwhat"
        twittering-timer-interval 600)
  ;; Don't kill the twittering buffer, just bury it
  (define-key twittering-mode-map (kbd "q") 'bury-buffer))

;; Mark passive voice, duplicate words and weasel words
(use-package writegood-mode
  :straight (:host github :repo "bnbeckwith/writegood-mode")
  :hook ((text-mode) . writegood-mode))

;; Mark E′ violations
(use-package eprime-mode
  :load-path "~/src/thornjad/eprime-mode"
  :after (general)
  :hook ((text-mode) . eprime-mode)
  :config
  (aero-leader-def
    "tp" 'eprime-check-buffer))


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

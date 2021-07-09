;;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2021 Jade Michael Thornton
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
;; This is simply a heap of miscellaneous, low-config packages.

;;; Code:

(require 'aero-prelude)

(use-package esup :straight t :defer t)

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

(use-package counsel-spotify :straight t
  :after (counsel general)
  :commands (counsel-spotify-toggle-play-pause
             counsel-spotify-next
             counsel-spotify-previous)
  :init
  (dolist (x '("toggle-play-pause" "next" "previous"
               "search-playlist" "search-track" "search-artist" "search-album"
               "search-tracks-by-artist" "search-tracks-by-album"))
    (eval
     `(defun ,(intern (concat "aero/spotify-" x)) ()
        (interactive)
        (aero/local! (funcall-interactively #',(intern (concat "counsel-spotify-" x))))
        (aero/log-info (concat "aero/spotify " ,x)))))

  (aero-leader-def
    "as" '(:ignore t :which-key "spotify")
    "asp" '(aero/spotify-toggle-play-pause :which-key "play/pause")
    "asn" '(aero/spotify-next :which-key "next")
    "asP" '(aero/spotify-previous :which-key "previous")
    "ass" '(:ignore t :wk "search")
    "assp" '(aero/spotify-search-playlist :wk "playlist")
    "asst" '(aero/spotify-search-track :wk "track")
    "assT" '(:ignore t :wk "search tracks by")
    "assTa" '(aero/spotify-search-track-by-artist :wk "search tracks by artist")
    "assTb" '(aero/spotify-search-track-by-album :wk "search tracks by album")
    "assa" '(aero/spotify-search-artist :wk "artist")
    "assb" '(aero/spotify-search-album :wk "album")))


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

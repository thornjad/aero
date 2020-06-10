;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
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

(require 'aero-lib)

(use-package counsel-spotify :straight t :defer 5
  :after counsel
  :config
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

(provide 'aero-spotify)

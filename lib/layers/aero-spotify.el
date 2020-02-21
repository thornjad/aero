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

(use-package counsel-spotify
	:disabled t
  :load-path "lib/packages/counsel-spotify"
  :after counsel
  :commands counsel-spotify-play
  :config
  (general-define-key
   :states 'normal
   :prefix "SPC"
    "as" '(:ignore t :which-key "spotify")
    "asp" '(counsel-spotify-toggle-play-pause :which-key "play/pause")
    "asn" '(counsel-spotify-next :which-key "next")
    "asP" '(counsel-spotify-previous :which-key "previous")
    "ass" 'counsel-spotify-search-track
    "asa" 'counsel-spotify-search-artist
    "asA" 'counsel-spotify-search-album))

(provide 'aero-spotify)

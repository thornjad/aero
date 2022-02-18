;;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2022 Jade Michael Thornton
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
  :straight (:host gitlab :repo "thornjad/eprime-mode" :branch "main")
  :after (general)
  :commands (eprime-check-buffer
             eprime-mode)
  :init
  (aero-leader-def
    "tp" 'eprime-check-buffer
    "tP" 'eprime-mode))

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

(use-package unmodified-buffer :defer 1
  :straight (:host github :repo "arthurcgusmao/unmodified-buffer")
  :hook ((prog-mode text-mode) . unmodified-buffer-mode))

;; Games

(use-package asm-blox
  :straight (asm-blox :host github :repo "zkry/asm-blox")
  :after (general)
  :commands (asm-blox)
  :init (aero-leader-def "aga" 'asm-blox))

;; other crap

;;; http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/
(defun buffer-mode-histogram ()
  "Display a histogram of emacs buffer modes."
  (interactive)
  (let* ((totals ())
         (buffers (buffer-list()))
         (total-buffers (length buffers))
         (ht (make-hash-table :test 'equal)))
    (save-excursion
      (dolist (buffer buffers)
        (set-buffer buffer)
        (let
            ((mode-name (symbol-name major-mode)))
          (puthash mode-name (1+ (gethash mode-name ht 0)) ht))))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "Buffer mode histogram"
      (princ (format "%d buffers open, in %d distinct modes\n\n"
                     total-buffers (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item)))
          (if (equal (substring key -5) "-mode")
              (setq key (substring key 0 -5)))
          (princ (format "%2d %20s %s\n" count key
                         (make-string count ?+))))))))

(use-package md4rd :straight t
  :after (general evil)
  :commands (md4rd)
  :init (aero-leader-def "ar" 'md4rd)
  :config
  (evil-define-key 'normal md4rd-mode-map
    "S" 'md4rd-jump-to-subs
    "Q" 'kill-current-buffer))

(provide 'aero-heap)

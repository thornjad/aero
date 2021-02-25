;; -*- lexical-binding: t -*-
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

(require 'aero-prelude)

;; Elfeed used to live here, see git history if interested


(use-package hackernews
  ;; local
  :commands hackernews)

(use-package pocket-reader :after (general)
	:commands pocket-reader
  :init
  (general-define-key
   :states 'normal
   :prefix "SPC"
   "aR" '(:ignore t :which-key "pocket")
   "aRr" 'pocket-reader
   "ar" 'pocket-reader
   "aRs" 'pocket-reader-add-link)
  :config
  (evil-set-initial-state 'pocket-reader-mode 'emacs)
  (setq pocket-reader-open-url-default-function 'eww))

(use-package xkcd :straight t
  :commands xkcd)

(provide 'aero-news)

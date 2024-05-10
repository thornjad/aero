;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2024 Jade Michael Thornton
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

(defvar aero/thornlog-elfeed-directory (expand-file-name "elfeed/" aero/thornlog-path)
  "The directory where elfeed will store its database and other files.")

(package! elfeed "skeeto/elfeed"
  :commands elfeed
  :after (general evil)
  :custom
  (elfeed-search-title-max-width 120)
  (elfeed-db-directory aero/thornlog-elfeed-directory)
  (elfeed-search-filter "+unread")
  :config
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (evil-set-initial-state 'elfeed-show-mode 'normal))

;; lets us use an elfeed.org file to manage our feeds and their tags
(package! elfeed-org :localpackage
  :after elfeed
  :custom (aero-elfeed-org-file (expand-file-name "elfeed.org" aero/thornlog-elfeed-directory))
  :config (aero-elfeed-org))



(package! pocket-reader "alphapapa/pocket-reader.el"
  :after (general)
  :commands (pocket-reader)
  :custom
  (pocket-reader-open-url-default-function #'eww)
  :init (aero-leader-def "aP" 'pocket-reader)
  :config
  ;; Evil messes with all the bindings, so we'll use the defaults in emacs mode.
  (evil-set-initial-state 'pocket-reader-mode 'emacs))

(package! hackernews "clarete/hackernews.el"
  :after (general)
  :commands (hackernews)
  :init (aero-leader-def "an" 'hackernews))



(provide 'aero-news)

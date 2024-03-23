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

(package! elfeed "skeeto/elfeed"
  :commands elfeed
  :after (general evil)
  :custom
  (elfeed-feeds
   '("https://sachachua.com/blog/category/emacs/feed/"
     "https://nullprogram.com/feed/"
     "http://www.wilfred.me.uk/rss.xml"
     "http://endlessparentheses.com/atom.xml"
     "http://irreal.org/blog/?feed=rss2"
     "http://emacshorrors.com/feed.atom"
     "https://cscheerleader.com/feed"
     "https://cestlaz.github.io/rss.xml"
     "https://jachinrupe.name/index.xml"
     "http://lambda-the-ultimate.org/rss.xml"
     "https://rachelbythebay.com/w/atom.xml"
     "http://matt.might.net/articles/feed.rss"
     "https://feeds.feedburner.com/typepad/krisdedecker/lowtechmagazineenglish"
     "https://secularbuddhism.com/feed"
     "https://secularbuddhism.org/category/articles/feed"
     "https://www.everydaybuddhist.org/blog/atom.xml"
     "https://tricycle.org/trikedaily/feed"
     "https://www.jquiambao.com/feed.rss"
     "https://blog.jmthornton.net/feed/jade.atom"))
  (elfeed-search-title-max-width 120)

  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)

  (general-define-key
   :keymaps 'elfeed-search-mode-map
   "R" 'elfeed-update
   "j" 'next-line
   "k" 'previous-line)

  (general-define-key
   :keymaps 'elfeed-show-mode-map
   "j" 'next-line
   "k" 'previous-line
   "o" 'elfeed-search-browse-url
   "SPC" nil ; don't scroll
   "TAB" 'elfeed-show-next-link
   "C-u" 'evil-scroll-up
   "C-d" 'evil-scroll-down))

(package! elfeed-protocol "fasheng/elfeed-protocol"
  :after (elfeed)
  :custom (elfeed-protocol-enabled-protocols '(fever newsblur owncloud ttrss))
  :config (elfeed-protocol-enable))



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

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
  (elfeed-search-title-max-width 120)
  (elfeed-db-directory (expand-file-name "elfeed/" aero/thornlog-path))
  (elfeed-search-filter "+unread")
  (elfeed-feeds
   '(("https://sachachua.com/blog/category/emacs/feed/" emacs)
     ("https://nullprogram.com/feed/" emacs)
     ("http://www.wilfred.me.uk/rss.xml" emacs)
     ("http://endlessparentheses.com/atom.xml" emacs)
     ("https://lifeofpenguin.blogspot.com/feeds/posts/default" emacs)
     ("https://protesilaos.com/codelog.xml" emacs)
     ("http://irreal.org/blog/?feed=rss2" emacs)
     ("http://emacshorrors.com/feed.atom" emacs)
     ("http://howardism.org/index.xml" emacs)
     ("http://www.masteringemacs.org/feed/" emacs)
     ("https://takeonrules.com/index.atom" emacs)
     "https://fy.blackhats.net.au/rss.xml"
     ("https://akib.codeberg.page/blog.xml" emacs)
     "https://www.wheresyoured.at/rss/"
     "https://cscheerleader.com/feed"
     "https://julesjacobs.com/feed.xml"
     "https://sameoldzen.blogspot.com/feeds/posts/default"
     "https://nora.codes/index.xml"
     "https://andrewkelley.me/rss.xml"
     "https://blog.andrewcantino.com/feed.xml"
     "https://bernsteinbear.com/feed.xml"
     "https://drewdevault.com/blog/index.xml"
     "https://danielchasehooper.com/feed.xml"
     "https://matklad.github.io/feed.xml"
     "https://danluu.com/atom.xml"
     "https://without.boats/index.xml"
     "https://www.defmacro.org/feed.xml"
     "https://cestlaz.github.io/rss.xml"
     "http://hardcorezen.info/feed"
     "https://jachinrupe.name/index.xml"
     "https://blag.xkcd.com/feed/"
     "https://rachelbythebay.com/w/atom.xml"
     "http://matt.might.net/articles/feed.rss"
     "https://solar.lowtechmagazine.com/posts/index.xml"
     "https://secularbuddhism.org/category/articles/feed"
     "https://www.everydaybuddhist.org/blog/atom.xml"
     "https://www.lionsroar.com/feed"
     "https://maia.crimew.gay/feed.xml"
     "https://jvns.ca/atom.xml"
     "https://acoup.blog/feed"
     "http://www.aaronsw.com/2002/feeds/pgessays.rss"
     "https://ntietz.com/atom.xml"
     "https://tonsky.me/atom.xml"
     ("https://oremacs.com/atom.xml" emacs)
     "https://www.jquiambao.com/feed.rss"
     "https://www.construction-physics.com/feed"
     "https://blog.jmthornton.net/feed/jade.atom"))

  :config
  (evil-set-initial-state 'elfeed-search-mode 'normal)
  (evil-set-initial-state 'elfeed-show-mode 'normal))



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

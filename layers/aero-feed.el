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

(use-package elfeed :ensure t
  :commands elfeed
  :init
  (setq elfeed-use-curl t)
  (setq elfeed-feeds
        '(
          ;; emacs blogs
          ("https://nullprogram.com/feed/" cs emacs)
          ("http://www.wilfred.me.uk/rss.xml" cs emacs)
          ("https://oremacs.com/atom.xml" cs emacs)
          ("https://emacsredux.com/atom.xml" cs emacs)
          ("https://scripter.co/atom.xml" cs emacs)
          ("https://bling.github.io/index.xml" cs emacs)
          ("https://www.masteringemacs.org/feed" cs emacs)
          ("http://endlessparentheses.com/atom.xml" cs emacs)
          ("https://acidwords.com/feed.xml" cs emacs)
          ("http://irreal.org/blog/?feed=rss2" cs emacs)
          ("http://emacshorrors.com/feed.atom" cs emacs)

          ;; general blogs
          ("https://www.defmacro.org/feed.xml" cs)
          ("http://www.philipzucker.com/feed/" cs)
          ("https://cestlaz.github.io/rss.xml" cs edu)
          ("https://modelingwithdata.org/modeling.xml" cs)
          ("http://cachestocaches.com/feed/" cs)
          ("https://protonmail.com/blog/feed/")
          ("https://briancarper.net/feed" cs)
          ("https://jachinrupe.name/index.xml")
          ("https://kotka.de/blog/index.rss" cs)
          ("https://blag.xkcd.com/feed/")
          ("http://normanmaurer.me/blog.atom" cs)
          ("https://whatthefuck.computer/rss.xml" cs)
          ("https://lcamtuf.blogspot.com/feeds/posts/default" cs)
          ("https://feeds2.feedburner.com/CodersTalk" cs)
          ("https://feeds.feedburner.com/codinghorror" cs)
          ("http://lambda-the-ultimate.org/rss.xml" cs)
          ("https://danluu.com/atom.xml" cs)
          ("https://jvns.ca/atom.xml" cs)
          ("http://newartisans.com/rss.xml" cs)
          ("https://rachelbythebay.com/w/atom.xml" cs)
          ("https://codewords.recurse.com/feed.xml" cs)
          ("http://akaptur.com/atom.xml" cs)
          ("https://www.evanjones.ca/index.rss" cs)
          ("http://neverworkintheory.org/feed.xml" cs)
          ("https://feeds.feedburner.com/GustavoDuarte" cs)
          ("https://blog.regehr.org/feed" cs)
          ("https://www.snellman.net/blog/rss-index.xml" cs)
          ("https://eli.thegreenplace.net/feeds/all.atom.xml" cs)
          ("http://idea.popcount.org/rss.xml" cs)
          ("https://flak.tedunangst.com/rss" cs)
          ("http://bit-player.org/feed" cs)
          ("https://blog.computationalcomplexity.org/feeds/posts/default" cs)
          ("http://matt.might.net/articles/feed.rss" cs)
          ("https://blog.juliobiason.net/rss.xml" cs)

          ;; news
          ("https://thefeature.net/rss" news)
          ("https://opensource.com/feed" news)
          ("https://spectrum.ieee.org/rss/computing/fulltext" news cs)
          ("https://spectrum.ieee.org/rss/green-tech/fulltext" news)
          )) ; left on new line for easy editing

  :config
  ;; increase title width for papers
  (setq elfeed-search-title-max-width 120)

  (define-key elfeed-show-mode-map (kbd "j") 'next-line)
  (define-key elfeed-show-mode-map (kbd "k") 'previous-line)

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)))

(provide 'aero-feed)

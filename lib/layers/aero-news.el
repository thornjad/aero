;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
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


(use-package elfeed :ensure t
  :commands elfeed
  :init
  (general-define-key
   :states 'normal
   :prefix "SPC"
    "af" 'elfeed)
  (setq elfeed-use-curl t)
  (setq
   elfeed-feeds
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
     ("http://howardism.org/index.xml" cs emacs)
     ("https://emacsair.me/feed.xml" cs emacs)
     ("https://iloveemacs.wordpress.com/feed/" cs emacs)
     ("https://sachachua.com/blog/category/emacs/feed/" cs emacs)
     ("https://feeds.feedburner.com/SanityInc" cs emacs)
     ("https://axisofeval.blogspot.com/feeds/posts/default" cs emacs)
     ("http://mbork.pl/?action=rss" cs emacs)

     ;; general blogs
     ("https://arp242.net/feed.xml" cs)
     ("https://cscheerleader.com/feed" cs)
     ("https://julesjacobs.github.io/feed.xml" cs)
     ("http://standardsandfreedom.net/index.php/feed/" cs)
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
     ("https://alistapart.com/main/feed/" cs tech)
     ("http://algosaur.us/feed/" cs)
     ("http://alyssafrazee.com/feed.xml" cs tech)
     ("https://amplab.cs.berkeley.edu/feed/" tech)
     ("http://www.benfrederickson.com/atom.xml" cs)
     ("https://notstatschat.tumblr.com/rss" cs)
     ("https://feeds.feedburner.com/bocoup" cs)

     ;; news
     ("https://thefeature.net/rss" news)
     ("https://opensource.com/feed" news tech)
     ("https://spectrum.ieee.org/rss/computing/fulltext" news cs tech)
     ("https://spectrum.ieee.org/rss/green-tech/fulltext" news)
     ("https://blog.mozilla.org/feed/" cs news)

     ;; rest
     ("https://feeds.feedburner.com/typepad/krisdedecker/lowtechmagazineenglish" tech)
     ("https://modernstoicism.com/feed/" stoic)
     )) ; left on new line for easy editing

  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)

  ;; increase title width for papers
  (setq elfeed-search-title-max-width 120)

  (general-define-key
   :keymaps 'elfeed-search-mode-map
    "R" 'elfeed-update)

  (general-define-key
   :keymaps 'elfeed-show-mode-map
   "j" 'next-line
   "k" 'previous-line
   "o" 'elfeed-search-browse-url
   "TAB" 'elfeed-show-next-link
   "C-u" 'scroll-down
   "C-d" 'scroll-up))


(use-package hackernews
  ;; local
  :commands hackernews)


(use-package pocket-reader :ensure t
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

(provide 'aero-news)

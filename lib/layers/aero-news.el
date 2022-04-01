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

(use-package elfeed :straight t
  :commands elfeed
  :after (general)
  :init
  (setq-default elfeed-use-curl t)
  (setq-default
   elfeed-feeds
   '(("https://sachachua.com/blog/category/emacs/feed/" emacs)
     ("https://nullprogram.com/feed/" emacs)
     ("http://www.wilfred.me.uk/rss.xml" emacs)
     ("https://oremacs.com/atom.xml" emacs)
     ("https://emacsredux.com/atom.xml" emacs)
     ("http://endlessparentheses.com/atom.xml" emacs)
     ("http://irreal.org/blog/?feed=rss2" emacs)
     ("http://emacshorrors.com/feed.atom" emacs)
     ("http://howardism.org/index.xml" emacs)
     ("https://feeds.feedburner.com/SanityInc" emacs)
     ("http://www.lunaryorn.com/feed.atom" emacs)
     ("http://www.masteringemacs.org/feed/" emacs)

     ("https://cscheerleader.com/feed")
     ("https://julesjacobs.github.io/feed.xml")
     ("http://standardsandfreedom.net/index.php/feed/")
     ("https://www.defmacro.org/feed.xml")
     ("http://www.philipzucker.com/feed/")
     ("https://cestlaz.github.io/rss.xml" edu)
     ("https://jachinrupe.name/index.xml")
     ("https://blag.xkcd.com/feed/")
     ("https://whatthefuck.computer/rss.xml")
     ("https://feeds.feedburner.com/codinghorror")
     ("http://lambda-the-ultimate.org/rss.xml")
     ("https://rachelbythebay.com/w/atom.xml")
     ("http://neverworkintheory.org/feed.xml")
     ("https://blog.computationalcomplexity.org/feeds/posts/default")
     ("http://matt.might.net/articles/feed.rss")

     ("https://thefeature.net/rss" news)
     ("https://opensource.com/feed" news tech)
     ("https://blog.mozilla.org/feed/" news)
     ("https://protonmail.com/blog/feed/" news)

     ("http://feeds.feedburner.com/ClojureAndMe" clojure)
     ("http://clojure.com/blog/atom.xml" clojure)
     ("http://feeds.feedburner.com/disclojure" clojure)

     ("https://feeds.feedburner.com/typepad/krisdedecker/lowtechmagazineenglish" tech)
     ("https://modernstoicism.com/feed/" philosophy)
     ("https://secularbuddhism.com/feed" philosophy)))

  :config
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)

  ;; increase title width for papers
  (setq-default elfeed-search-title-max-width 120)

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

(use-package elfeed-summary
  :after (general)
  :straight (:host github :repo "SqrtMinusOne/elfeed-summary")
  :config (aero-leader-def "af" 'elfeed-summary))



(use-package pocket-reader :straight t
  :after (general)
  :commands (pocket-reader)
  :custom
  (pocket-reader-open-url-default-function #'eww)
  :init (aero-leader-def "aP" 'pocket-reader)
  :config
  ;; Evil messes with all the bindings, so we'll use the defaults in emacs mode.
  (evil-set-initial-state 'pocket-reader-mode 'emacs))

(use-package hackernews :straight t
  :after (general)
  :commands (hackernews)
  :init (aero-leader-def "an" 'hackernews))



(provide 'aero-news)

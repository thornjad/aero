# elfeed-org - Configure elfeed with one or more org-mode files


A system for keeping elfeed RSS feeds and their tags in an org file. This package is based on
elfeed-org by Remy Honig, but has been simplified and updated to suit my tastes.

As this package is modified from the original elfeed-org, which carried a GPLv3 license, this
package is also licensed under the GPLv3. This license does not extend to the rest of Aero Emacs,
see the top-level README for more information on Aero Emacs. This version of this package removes
the option to use any future versions of the GPL, only version 3. I do not personally like the
GPL and do not wish to automatically upgrade to future versions of it.

Text under headlines is ignored

## Example

``` org
* https://example.com/feed.xml  :feedtag:
* Emacs  :emacs:
** https://sachachua.com/blog/category/emacs/feed/
** https://planet.emacslife.com/atom.xml
   :PROPERTIES:
   :blog-title: Sacha Chua's Emacs Blog
   :END:
Add a blog title to the feed with a :blog-title: property
```


---
Converted from `elfeed-org.el` by [_el2md_](https://gitlab.com/thornjad/el2md).

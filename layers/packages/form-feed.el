;;; form-feed.el --- Display ^L glyphs as horizontal lines
;;
;; Copyright (c) 2019 Jade Michael Thornton
;; Copyright (c) 2014-2016 Vasilij Schneidermann
;;
;; This file is NOT part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This minor mode displays form feed characters (page breaks) as a syngle
;; horizontal line, spanning the window.
;;
;; There are a bunch of ways of attacking this problem, one of the more obscure
;; ones is manipulating the display table of every window displaying the buffer.
;; Unfortunately this approach is limited to replacing a glyph with an array of
;; other glyphs, but guaranteed to work on non-graphical display as well. The
;; other approach is putting an overlay or text property over the glyph which
;; manipulates its look. Since a face on its own won't do the trick, this
;; package uses a lesser known feature of font-lock that allows one to add text
;; properties as part of the face definition associated with the page delimiter
;; glyph and tells it to remove those on fontification changes to make sure
;; disabling works equally well. This also means that while this package is
;; conceptually very simple and non-invasive, it might not work on non-graphical
;; displays. As a workaround this packages makes Emacs use underlining instead
;; of strike-through on such displays.
;;
;; The implementation of display lines was inspired by the magic-buffer package,
;; but did eventually remove its "cursor kicking" due to a rather puzzling bug.
;;
;; This package is a slightly customized version ofVasilij Schneidermann's
;; form-feed package. That package's readme is a good read:
;; https://github.com/wasamasa/form-feed.
;;
;;; Code:


;;; variables

(defgroup form-feed nil
  "Turn ^L glyphs into horizontal lines."
  :prefix "form-feed-"
  :group 'faces)

(defface form-feed-line
  '((((type graphic)
      (background light)) :strike-through "black")
    (((type graphic)
      (background dark)) :strike-through "white")
    (((type tty)) :inherit font-lock-comment-face :underline t))
  "Face for form-feed-mode lines."
  :group 'form-feed)

(defcustom form-feed-line-width t
  "Width of the form feed line.
It may be one of the following values:

t: Full width.

floating point number: Ratio of full width.  A value of 0.5 would
use half the width.

positive integer number: Width as measured in columns.  A value
of 80 would use a 80 characters wide line.

negative integer number: Full width minus specified number of
columns.  A value of -1 would leave the last column empty."
  :type '(choice (const :tag "Full width" t)
                 (float :tag "Ratio")
                 (integer :tag "Columns"))
  :group 'form-feed)

(defvar form-feed--line-width
  (cond
   ((integerp form-feed-line-width)
    (if (>= form-feed-line-width 0)
        form-feed-line-width
      `(- text ,(abs form-feed-line-width))))
   ((floatp form-feed-line-width)
    `(,form-feed-line-width . text))
   (t 'text)))

(defcustom form-feed-extra-properties nil
  "List of additional text properties to add to form feeds."
  :type '(plist)
  :group 'form-feed)

(defvar form-feed--font-lock-face
  ;; NOTE see (info "(elisp) Search-based fontification") and the
  ;; `(MATCHER . FACESPEC)' section
  `(face form-feed-line display (space :width ,form-feed--line-width)
         ,@form-feed-extra-properties))

(defvar form-feed--font-lock-keywords
  ;; NOTE see (info "(elisp) Search-based fontification") and the
  ;; `(MATCHER . SUBEXP-HIGHLIGHTER)' section
  `((,page-delimiter 0 form-feed--font-lock-face t)))

(defcustom form-feed-lighter " ^L"
  "Lighter for `form-feed-mode'."
  :type 'string
  :group 'form-feed
  :risky t)


;;; Functions

(defun form-feed--add-font-lock-keywords ()
  "Add buffer-local keywords to display page delimiter lines.
Make sure the special properties involved get cleaned up on
removal of the keywords via
`form-feed-remove-font-lock-keywords'."
  (font-lock-add-keywords nil form-feed--font-lock-keywords)
  (set (make-local-variable 'font-lock-extra-managed-props)
       (append `(display ,@form-feed-extra-properties)
               font-lock-extra-managed-props)))

(defun form-feed--remove-font-lock-keywords ()
  "Remove buffer-local keywords displaying page delimiter lines."
  (font-lock-remove-keywords nil form-feed--font-lock-keywords)
  (dolist (property (append '(display) form-feed-extra-properties))
    (setq font-lock-extra-managed-props
          (delq property font-lock-extra-managed-props))))

;;;###autoload
(define-minor-mode form-feed-mode
  "Toggle form-feed-mode.

This minor mode displays page delimiters which usually appear as ^L
glyphs on a single line as horizontal lines spanning the entire
window."
  :lighter form-feed-lighter
  (if form-feed-mode
      (form-feed--add-font-lock-keywords)
    (form-feed--remove-font-lock-keywords))

  (when (called-interactively-p 'interactive)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (font-lock-fontify-buffer))))

(provide 'form-feed)
;;; form-feed.el ends here

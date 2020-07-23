;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2020 Jade Michael Thornton
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

;;; Commentary:

;; This theme is a wrapper around `tao-theme' which acts in a very similar way
;; to `tao-yang-theme'. It provides greater support for Aero styling without
;; needing to resort to re-implementing the entire theme.

;;; Code:

(require 'tao-theme)

(deftheme aero-tao-yang "Aero version of the light Tao color theme")
(tao-with-color-variables
  tao-theme-yang-palette
  (tao-apply-custom-theme 'aero-tao-yang)
  (custom-theme-set-faces
   'aero-tao-yang
   ;; `(aero/modeline-window-number ((t (:foreground ,aero-fg :background ,aero-grey2))))
   ;; `(aero/modeline-major-mode-active ((t (:foreground ,aero-grey5 :bold t :background ,aero-grey1))))
   ;; `(aero/modeline-major-mode-inactive ((t (:foreground ,aero-grey3 :bold t :background ,aero-grey1))))
   )
  )

(provide-theme 'aero-tao-yang)

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

(use-package markdown-mode :ensure t
  :mode "\\.\\(md\\|markdown\\)")

(use-package nov :ensure t
  :mode "\\.epub\\'"
  :init
  (setq nov-text-width 80))

(provide 'aero-markup)

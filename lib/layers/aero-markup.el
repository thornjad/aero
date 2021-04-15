;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019, 2021 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(require 'aero-prelude)

(use-package markdown-mode :straight t
  :mode (("\\`README\\.md\\'" . gfm-mode)
        ("github\\.com.*\\.txt\\'" . gfm-mode)
        ("\\.md\\'"          . markdown-mode)
        ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh"))
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  :config (add-hook 'markdown-mode-hook 'variable-pitch-mode))

(use-package markdown-toc :straight t
  :commands (markdown-toc-generate-toc markdown-toc-refresh-toc))

(use-package yaml-mode :straight t
  :mode "\\.ya?ml\\'")

(use-package synosaurus
  :after (general)
  :init
  (setq-default synosaurus-backend 'synosaurus-backend-wordnet)
  (add-hook 'after-init-hook #'synosaurus-mode))

(use-package nov :straight t
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-text-width fill-column))

(provide 'aero-markup)

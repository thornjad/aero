;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(use-package python
  :ensure python-mode
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.pyx\\'" . python-mode)
   ("\\.wsgi$" . python-mode))
  :config
  (require 'virtualenvwrapper))

(use-package hy-mode :ensure t
  :mode "\\.hy\\'"
  :config
  (require 'virtualenvwrapper))

(use-package virtualenvwarapper
  :load-path "lib/packages/virtualenvwarapper.el/"
  :defer t
  :config
  (defvar venv-location)
  (setq venv-location "/path/to/your/virtualenvs/")
  (venv-initialize-eshell))

(provide 'aero-python)

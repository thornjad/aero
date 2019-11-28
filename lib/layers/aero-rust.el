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

(setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))

(use-package rust-mode :straight t
  :mode "\\.rs\\'"
  :config
  (require 'company)
  (defvar company-tooltip-align-annotations)
  (declare-function company-indent-or-complete-common "company")
  (setq company-tooltip-align-annotations t)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode t))))

(use-package racer :straight t
  :defines (racer-cmd
            racer-rust-src-path)
  :hook (rust-mode . racer-mode)

  :config
  ;; TODO make these more better
  (setq
   racer-cmd "~/.cargo/bin/racer"
   racer-rust-src-path
   "~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")
  (add-hook 'racer-mode-hook #'company-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package company-racer :straight t
  :after company
  :config
  (require 'company)
  (defvar company-backends)
  (push 'company-racer company-backends))

(use-package flycheck-rust :straight t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package cargo :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package toml-mode :straight t
  :mode "\\.toml\\'")

(provide 'aero-rust)

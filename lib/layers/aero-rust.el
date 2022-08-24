;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2020 Jade Michael Thornton
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

(setenv "PATH" (concat "~/.cargo/bin:" (getenv "PATH")))

(package! rust-mode :auto
  :mode "\\.rs\\'"
  :config
  (require 'company)
  (defvar company-tooltip-align-annotations)
  (declare-function company-indent-or-complete-common "company")
  (setq company-tooltip-align-annotations t)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode t))))

(package! racer :auto
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

(package! company-racer :auto
  :hook (rust-mode . racer-mode)
  :after company
  :config
  (require 'company)
  (defvar company-backends)
  (push 'company-racer company-backends))

(package! flycheck-rust :auto
  :hook (rust-mode . flycheck-rust-setup))

(package! cargo :auto
  :commands cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(package! toml-mode :auto
  :mode "\\(\\.toml\\|Cargo\\.lock\\)\\'")

(provide 'aero-rust)

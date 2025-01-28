;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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
;;
;;; Commentary:
;;
;; Provides a derived mode for yarn.lock files
;;
;;; Code:

(defvar yarn-lock-mode-syntax-table
  (let ((syntable (make-syntax-table)))
    (modify-syntax-entry ?# "<" syntable)
    (modify-syntax-entry ?\n ">" syntable)
    (modify-syntax-entry ?\" "\"" syntable)
    syntable))

(defvar yarn-lock-mode-package-re "\\(^\\|,\\s-\\)\\([a-zA-Z-_0-9]+\\)@")
(defvar yarn-lock-mode-dependencies-re "\\s-\\{4,\\}\\([a-zA-Z-_0-9]+\\)\\s-")
(defvar yarn-lock-mode-attributes-re
  (regexp-opt '("version" "resolved" "dependencies" "integrity")))
(defvar yarn-lock-mode-font-lock-defaults
  `((,yarn-lock-mode-attributes-re . '((t :inherit font-lock-builtin-face)))
    (,yarn-lock-mode-package-re . (2 '((t :inherit bold)) t)) ;; Direct deps
    (,yarn-lock-mode-dependencies-re . (1 '((t :inherit bold)) t)) ;; Dep of another dep (nested)
    ))
(define-derived-mode yarn-lock-mode text-mode "Yarn Lock"
  "Simple mode for yarn.lock."
  :syntax-table yarn-lock-mode-syntax-table
  (setq font-lock-defaults '(yarn-lock-mode-font-lock-defaults)
        buffer-read-only t))
(add-to-list 'auto-mode-alist '("yarn\\.lock\\'" . yarn-lock-mode))

(provide 'aero-yarn-lock)

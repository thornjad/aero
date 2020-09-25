;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
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
;; This file is not part of GNU Emacs

(use-package sql :defer t
  :commands (sql-connect)
  :mode ("\\.\\(sqlite_\\)?sql\\'" . sql-mode)

  :init
  (aero-leader-def
    "Sc" 'sql-connect)

  :config
  (aero-mode-leader-def
    :keymaps 'sql-mode-map
    "b" 'sql-send-buffer
    "B" 'aero/sql-send-buffer-and-focus
    "r" 'sql-send-region
    "R" 'aero/sql-send-region-and-focus
    "p" 'sql-send-paragraph
    "P" 'aero/sql-send-paragraph-and-focus
    "s" 'sql-send-string
    "S" 'aero/sql-send-string-and-focus)

	;; for sql comint
	(add-to-list 'same-window-buffer-names "*SQL: *")
  (add-hook 'sql-interactive-mode-hook #'evil-insert-state)

  (defun aero/sql-send-string-and-focus ()
    "Send a string to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (call-interactively 'sql-send-string)
      (evil-insert-state)))

  (defun aero/sql-send-buffer-and-focus ()
    "Send the buffer to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-buffer)
      (evil-insert-state)))

  (defun aero/sql-send-paragraph-and-focus ()
    "Send the paragraph to SQLi and switch to SQLi in `insert state'."
    (interactive)
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-paragraph)
      (evil-insert-state)))

  (defun aero/sql-send-region-and-focus (start end)
    "Send region to SQLi and switch to SQLi in `insert state'."
    (interactive "r")
    (let ((sql-pop-to-buffer-after-send-region t))
      (sql-send-region start end)
      (evil-insert-state)))

  (defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))
  (add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook))

(provide 'aero-sql)

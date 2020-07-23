;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019-2020 Jade Michael Thornton
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

(use-package lsp-mode :straight t
  :defer t
  :after general
  :commands (lsp-install-server lsp lsp-deferred)

  :init
  (setq lsp-session-file (expand-file-name "lsp-session" aero-etc-dir)
        lsp-server-install-dir (expand-file-name "lsp/" aero-etc-dir)
        lsp-intelephense-storage-path (expand-file-name "lsp-intelephense/" aero-cache-dir)
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil)

  ;; Disable LSP's superfluous, expensive and/or debatably unnecessary features.
  ;; Some servers implement these poorly. Better to just rely on Emacs' native
  ;; mechanisms and make these opt-in.
  (setq lsp-enable-folding nil
        ;; HACK Fix https://github.com/hlissner/doom-emacs/issues/2911, until it
        ;; is resolved upstream. Links come in asynchronously from the server,
        ;; but lsp makes no effort to "select" the original buffer before laying
        ;; them down, so they could be rendered in the wrong buffer (like the
        ;; minibuffer).
        lsp-enable-links nil
        ;; Potentially slow
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-semantic-highlighting nil
        ;; Don't modify our code without our permission
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        ;; capf is the preferred completion mechanism for lsp-mode
        lsp-prefer-capf t)

;;   (defun aero--advice-lsp-mode-silence (format &rest args)
;;     "Silence needless diagnostic messages from `lsp-mode'. This is a
;; `:before-until' advice for several `lsp-mode' logging functions."
;;     (or
;;      ;; Messages we get when trying to start LSP (happens every time we open a
;;      ;; buffer).
;;      (member format `("No LSP server for %s(check *lsp-log*)."
;;                       "Connected to %s."
;;                       ,(concat
;;                         "Unable to calculate the languageId for current "
;;                         "buffer. Take a look at "
;;                         "lsp-language-id-configuration.")))
;;      ;; Errors we get from gopls for no good reason (I can't figure out why).
;;      ;; They don't impair functionality.
;;      (and (stringp (car args))
;;         (or (string-match-p "^no object for ident .+$" (car args))
;;            (string-match-p "^no identifier found$" (car args))))))
;;   (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
;;     (advice-add fun :before-until #'aero--advice-lsp-mode-silence))

  ;; Ignore yasnippet crap, since its not used
  (setq lsp-enable-snippet nil)

  ;; (aero/defadvice
  ;;     aero--lsp-run-from-node-modules (command)
  ;;   :filter-return lsp-resolve-final-function
  ;;   "Find LSP executables inside node_modules/.bin if present."
  ;;   (cl-block nil
  ;;     (prog1 command
  ;;       (when-let ((project-dir
  ;;                   (locate-dominating-file default-directory "node_modules"))
  ;;                  (binary
  ;;                   (aero/path-join
  ;;                    project-dir "node_modules" ".bin" (car command))))
  ;;         (when (file-executable-p binary)
  ;;           (cl-return (cons binary (cdr command))))))))

;;   (aero/defhook
;;       aero--lsp-teardown ()
;;     kill-emacs-hook
;;     "Ignore the LSP server getting killed. If we don't do this, then when killing
;; Emacs we may be prompted with whether we want to restart the LSP server that has
;; just been killed (which happens during Emacs shutdown)."
;;     (setq lsp-restart nil))

  ;; `lsp-mode' doesn't know about LaTeX yet.
  ;; (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; fix some bad regexps
  ;; (setq lsp-language-id-configuration
  ;;       (mapcar
  ;;        (lambda (link)
  ;;          (if (and (stringp (car link))
  ;;                 (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
  ;;              (cons
  ;;               (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
  ;;            link))
  ;;        lsp-language-id-configuration))
  )

(use-package lsp-ui :straight t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp :straight t
  :defer t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(provide 'aero-lsp)

;;; aero-assistant-stablelm.el --- StableLM API for Aero Assistant  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, 2024 Jade Michael Thornton
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
;; This package provides the API functions for Aero Assistant interface with
;; StableLM Chat.

;;; Code:

(defun aa--send-stablelm ()
  "Sends prompts to StableLM Chat."
  (let* ((prompt (aa--gather-prompts-stablelm)))))

(defun aa--gather-prompts-stablelm ()
  "Return a full prompt from chat history, prepended with a system prompt."
  (let ((prompts (aa--filter-history-prompts-format-stablelm
                  #'aa--valid-prompt-p
                  (or (and aa-max-entries
                           (seq-take aa--history aa-max-entries))
                      aa--history))))
    (when (not prompts)
      (user-error "Prompt history contains nothing to send."))
    (cons (list :role "system"
                :content (format "You are a large language model living in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely. Use Markdown formatting in all messages. Current date: %s" (format-time-string "%Y-%m-%d")))
          ;; Need to reverse so latest comes last
          (nreverse prompts))))

(defun aa--filter-history-prompts-format-stablelm (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (aa--format-stablelm-prompt (car hist))
              (aa--filter-history-prompts-format-gpt pred (cdr hist)))
      (aa--filter-history-prompts-format-gpt pred (cdr hist)))))

(defun aa--format-stablelm-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(provide 'aero-assistant-stablelm)
;;; aero-assistant-stablelm.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("aa-" . "aero/assistant-"))
;; End:

;;; aero-assistant-openai.el --- OpenAI API for Aero Assistant  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Jade Michael Thornton
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
;; GPT 3.5 and 4, and Davinci.
;;
;; Requires `aero/assistant-openai-api-key' to be set

;;; Code:

(defun aero/assistant--send-openai (model)
  "Send prompts to OpenAI MODEL."
  (unless aero/assistant-openai-api-key
    (user-error "Must set `aero/assistant-openai-api-key'"))
  (let* ((prompt (aero/assistant--gather-prompts-openai))
         (inhibit-message t)
         (message-log-max nil)
         (url-show-status aero/assistant-debug-mode)
         (url-show-headers aero/assistant-debug-mode)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" aero/assistant-openai-api-key))))
         (url-request-data (encode-coding-string
                            ;; https://platform.openai.com/docs/api-reference/chat/create
                            (json-encode `(:model ,model
                                           :messages [,@prompt]
                                           :temperature nil
                                           :max_tokens nil))
                            'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (let ((message (aero/assistant--register-response
                                    (aero/assistant--parse-response-openai (current-buffer)))))
                      (aero/assistant--display-message message)
                      (setq aero/assistant--busy-p nil)
                      (spinner-stop aero/assistant--spinner)
                      (kill-buffer)))
                  nil (not aero/assistant-debug-mode) nil)))

(defun aero/assistant--gather-prompts-openai ()
  "Return a full prompt from chat history, prepended with a system prompt."
  (let ((prompts (aero/assistant--filter-history-prompts-format-openai
                  #'aero/assistant--valid-prompt-p
                  (or (and aero/assistant-max-entries
                           (seq-take aero/assistant--history aero/assistant-max-entries))
                      aero/assistant--history))))
    (when (not prompts)
      (user-error "Prompt history contains nothing to send."))
    (cons (list :role "system"
                :content (format "I want you to act as a brilliant senior software engineer working in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely, and cite sources for factual claims. Use Markdown formatting in all messages. Current date: %s" (format-time-string "%Y-%m-%d")))
          ;; Need to reverse so latest comes last
          (nreverse prompts))))


(defun aero/assistant--filter-history-prompts-format-openai (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (aero/assistant--format-openai-prompt (car hist))
              (aero/assistant--filter-history-prompts-format-openai pred (cdr hist)))
      (aero/assistant--filter-history-prompts-format-openai pred (cdr hist)))))

(defun aero/assistant--format-openai-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(defun aero/assistant--parse-response-openai (buffer)
  "Parse the Assitant response in URL BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when aero/assistant-debug-mode (clone-buffer "*aero/assistant-error*" 'show))
      (if-let* ((status (buffer-substring (line-beginning-position) (line-end-position)))
                (json-object-type 'plist)
                (response
                 (progn
                   (forward-paragraph)
                   (let ((json-str
                          (decode-coding-string
                           (buffer-substring-no-properties (point) (point-max))
                           'utf-8)))
                     (condition-case nil
                         (json-read-from-string json-str)
                       (json-readtable-error 'json-read-error))))))

          (cond
           ((string-match-p "200 OK" status)
            (let* ((choices (aref (plist-get response :choices) 0))
                   (message (plist-get choices :message)))
              (if choices
                  (list :content (string-trim (substring-no-properties (plist-get message :content)))
                        :tokens (plist-get response :usage)
                        :time (plist-get response :created)
                        :stop (plist-get choices :finish_reason)
                        :status (substring-no-properties status))
                (list :error t :status "No message received"))))
           ((plist-get response :error)
            (let* ((error-plist (plist-get response :error))
                   (error-msg (plist-get error-plist :message))
                   (error-type (plist-get error-plist :type)))
              (list :error t :status (concat status ": " error-type))))
           ((eq response 'json-read-error)
            (list :error t :status (concat status ": Malformed JSON in response.")))
           (t (list :error t :status (concat status ": Could not parse HTTP response."))))
        (list :error t :status (concat status ": Could not parse HTTP response."))))))

(provide 'aero-assistant-openai)
;;; aero-assistant-openai.el ends here

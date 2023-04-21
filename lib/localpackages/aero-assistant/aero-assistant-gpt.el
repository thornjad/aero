;;; aero-assistant-gpt.el --- GPT API Functions for Aero Assistant  -*- lexical-binding: t; -*-
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
;; A simple markdown-based AI client for Aero Emacs.
;;
;; Requires `aero/assistant-openai-api-key' to be set

;;; Code:

(defcustom aero/assistant-openai-api-key nil
  "An OpenAI API key."
  :group 'aero/assistant
  :type 'string)

(defun aero/assistant--send-gpt-3.5-turbo ()
  "Send prompts to GPT 3.5 Turbo."
  (unless aero/assistant-openai-api-key
    (user-error "Must set `aero/assistant-openai-api-key'"))
  (let* ((prompt (aero/assistant--gather-prompts-gpt))
         (inhibit-message t)
         (message-log-max nil)
         (url-show-status aero/assistant-debug-mode)
         (url-show-headers aero/assistant-debug-mode)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " aero/assistant-openai-api-key))))
         (url-request-data (encode-coding-string
                            ;; https://platform.openai.com/docs/api-reference/chat/create
                            (json-encode `(:model "gpt-3.5-turbo"
                                           :messages [,@prompt]
                                           :temperature nil
                                           :max_tokens nil))
                            'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (let ((message (aero/assistant--register-response
                                    (aero/assistant--parse-response-gpt (current-buffer)))))
                      (aero/assistant--display-message message)
                      (setq aero/assistant--busy-p nil)
                      (spinner-stop aero/assistant--spinner)
                      (kill-buffer)))
                  nil (not aero/assistant-debug-mode) nil)))

(defun aero/assistant--gather-prompts-gpt ()
  "Return a full prompt from chat history, prepended with a system prompt."
  (let ((prompts (aero/assistant--filter-history-prompts-format
                  #'aero/assistant--valid-prompt-p
                  (or (and aero/assistant-max-entries
                           (seq-take aero/assistant--history aero/assistant-max-entries))
                      aero/assistant--history))))
    (when (not prompts)
      (user-error "Prompt history contains nothing to send."))
    (cons (list :role "system"
                :content (format "You are a large language model living in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely. Use Markdown formatting in all messages. Current date: %s" (format-time-string "%Y-%m-%d")))
          ;; Need to reverse so latest comes last
          (nreverse prompts))))

(defun aero/assistant--parse-response-gpt (buffer)
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

(provide 'aero-assistant-gpt)
;;; aero-assistant-gpt.el ends here

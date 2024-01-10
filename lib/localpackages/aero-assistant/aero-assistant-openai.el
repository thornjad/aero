;;; aero-assistant-openai.el --- OpenAI API for Aero Assistant  -*- lexical-binding: t; -*-
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
;; GPT 3.5 and 4, and Davinci.
;;
;; Requires `aa-openai-api-key' to be set

;;; Code:

(defconst aa-openai-system-prompt
  "You will act as a brilliant and experienced senior software engineer working in Emacs; you are a helpful assistant and a careful, wise programmer.
The user is a senior software engineer with limited time.
You treat the user's time as precious, but you are not afraid to ask for clarification when needed.
You do not repeat obvious things, including the user's query.
You care that the user improve their skills and understanding, and guide the user to go a level deeper and help them see patterns.
Respond concisely and cite sources for factual claims.
Do not explain code snippets unless asked to do so.
Use Markdown formatting liberally in all messages.
Always show code snippets in markdown blocks with language labels.
When using Python, assume the user is using version 3.9 or newer.
When using Typescript, assume the user is using version 4.8 or newer.
Current date: %s")

(defconst aa-commit-git-command
  '("diff" "--cached" "--summary" "-U10" "--no-color")
  "The git command given to Magit to get the commit diff.")

(defconst aa-commit-system-prompt
  "You are acting as a brilliant and experienced senior software engineer. The user will provide the result of running 'git %s'. You will suggest a commit message based on the diff. Do not respond with anything other than the commit message. The following describes guidelines for a proper commit message; you must follow them carefully at all times.

- The message MUST NEVER exceed 50 characters.
- The message MUST ALWAYS begin with a lower-case letter.
- The message MUST be in the imperative mood.
- The message must not end with a period, and should not end with any other punctuation.
- The message must not begin with a commit type (e.g. \"fix:\", \"feat:\", \"docs:\", etc.)
- The message must not include a commit body, you must respond with the commit message only.
- The message should not include file names.
- The message should avoid using the verb \"to be\".
- The message should not use the word \"refactor\".
- When a submodule is being updated, the commit message should mention the name of the submodule.
- Once again, the message must never ever exceed 50 characters.
")

(defun aa--send-openai (model)
  "Send prompts to OpenAI MODEL."
  (unless aa-openai-api-key (user-error "Must set `aa-openai-api-key'"))
  (let ((single-prompt (member model aa--dall-e-models)))
    (aa--send-openai-request
     model
     (aa--gather-prompts-openai single-prompt)
     (lambda (response)
       (let ((message (aa--register-response response)))
         (aa--display-message message)
         (markdown-display-inline-images)
         (setq aa--busy-p nil)
         (spinner-stop aa--spinner))))))

(defun aa--gen-commit-message-openai (model callback)
  "Generate a commit message and pass it to CALLBACK."
  (unless (require 'magit nil t) (user-error "This function requires `magit'"))
  (let* ((diff-lines (magit-git-lines "diff" "--cached"))
         (changes (string-join diff-lines "\n"))
         (system-prompt (format aa-commit-system-prompt (string-join aa-commit-git-command " ")))
         (message (list (list :role "system" :content system-prompt)
                        (list :role "user" :content changes))))
    (unless message (user-error "No changes to commit"))
    (message "Aero Assistant is generating a commit message...")
    (aa--send-openai-request
     model message
     (lambda (message)
       (funcall callback (aa--format-commit-message-content message))))))

(defun aa--format-commit-message-content (message)
  "Return MESSAGE with it's :content downcased."
  (let* ((content (plist-get message :content))
         (content (if (> (length content) 0)
                      (concat (downcase (substring content 0 1))
                              (substring content 1))
                    content))
         (content (if (string-suffix-p "." content)
                      (string-remove-suffix "." content)
                    content)))
    (plist-put message :content content)))

(defun aa--send-openai-request (model message callback)
  "Send MESSAGE to OpenAI MODEL and call CALLBACK with the response."
  (unless aa-openai-api-key (user-error "Must set `aa-openai-api-key'"))
  (let ((inhibit-message t)
        (message-log-max nil)
        (url-show-status aa-debug-mode)
        (url-show-headers aa-debug-mode)
        (url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(format "Bearer %s" aa-openai-api-key))))
        (url-request-data (aa--request-data model message)))

    (url-retrieve (aa--openapi-api-url model)
                  (lambda (_)
                    (funcall callback (aa--parse-response-openai model (current-buffer)))
                    (kill-buffer))
                  nil (not aa-debug-mode) nil)))

(defun aa--request-data (model message)
  "Get request data for MODEL with MESSAGE"
  (cond ((member model aa--dall-e-models)
         (aa--request-data-dall-e model message))
        ((member model aa--gpt-models)
         (aa--request-data-gpt model message))
        (t (user-error "Model %s not supported" model))))

(defun aa--openapi-api-url (model)
  "Get the OpenAI API URL for MODEL."
  (cond ((member model aa--dall-e-models)
         "https://api.openai.com/v1/images/generations")
        ((member model aa--gpt-models)
         "https://api.openai.com/v1/chat/completions")
        (t (user-error "Model %s not supported" model))))

(defun aa--request-data-dall-e (model message)
  "Get the request data for a DALL-E MODEL call with MESSAGE."
  (encode-coding-string
   (json-encode `(:model ,model
                  :prompt ,message
                  :n 1 ;; only 1 is supported for dall-e 3
                  :quality ,aa-dall-e-quality
                  :style ,aa-dall-e-style))
   'utf-8))

(defun aa--request-data-gpt (model messages)
  "Get the request data for a GPT MODEL call with MESSAGES."
  (encode-coding-string
   ;; https://platform.openai.com/docs/api-reference/chat/create
   (json-encode `(:model ,model
                  :messages [,@messages]
                  :temperature nil
                  :max_tokens nil))
   'utf-8))

(defun aa--gather-prompts-openai (single-prompt)
  "Return a full prompt from chat history, or last prompt if SINGLE-PROMPT."
  (let ((prompts (aa--filter-history-prompts-format-openai
                  #'aa--valid-prompt-p
                  (or (and aa-max-entries
                           (seq-take aa--history aa-max-entries))
                      aa--history))))
    (when (not prompts) (user-error "Prompt history contains nothing to send."))
    (if single-prompt
        (let ((last-prompt (car aa--history)))
          (when (string= "user" (plist-get last-prompt :role))
            (plist-get last-prompt :content)))
      (cons (list :role "system"
                  :content (format aa-openai-system-prompt (format-time-string "%Y-%m-%d")))
            ;; Need to reverse so latest comes last
            (nreverse prompts)))))

(defun aa--filter-history-prompts-format-openai (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (aa--format-openai-prompt (car hist))
              (aa--filter-history-prompts-format-openai pred (cdr hist)))
      (aa--filter-history-prompts-format-openai pred (cdr hist)))))

(defun aa--format-openai-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(defun aa--parse-response-openai (model buffer)
  "Parse the Assistant response for GPT in BUFFER for MODEL."
  (cond ((member model aa--dall-e-models)
         (aa--parse-response-openai-dall-e buffer))
        ((member model aa--gpt-models)
         (aa--parse-response-openai-gpt buffer))
        (t (user-error "Model %s not supported" model))))

(defun aa--parse-response-openai-dall-e (buffer)
  "Parse the Assistant response for DALL-E in BUFFER."
  (when (buffer-live-p buffer)
    (aa--show-debug-buffer buffer)
    (if-let ((status (aa--get-response-status buffer))
             (response (aa--get-response-json buffer)))
        (cond
         ((string-match-p "200 OK" status)
          (let ((response-data (aref (plist-get response :data) 0)))
            (list :content (string-trim  (plist-get response-data :url))
                  :revised_prompt (plist-get response-data :revised_prompt)
                  :show_image t
                  :time (plist-get response :created)
                  :status status)))

         ((plist-get response :error)
          (let* ((error-plist (plist-get response :error))
                 (error-msg (plist-get error-plist :message))
                 (error-type (plist-get error-plist :type)))
            (list :error t :status (concat status ": " error-type))))

         ((eq response 'json-read-error)
          (list :error t :status (concat status ": Malformed JSON in response.")))

         (t (list :error t :status (concat status ": Could not parse HTTP response."))))

      ;; if-let errored
      (list :error t :status (concat status ": Could not parse HTTP response.")))))

(defun aa--parse-response-openai-gpt (buffer)
  "Parse the Assistant response for GPT in BUFFER."
  (when (buffer-live-p buffer)
    (aa--show-debug-buffer buffer)
    (if-let ((status (aa--get-response-status buffer))
             (response (aa--get-response-json buffer)))
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
      (list :error t :status (concat status ": Could not parse HTTP response.")))))

(defun aa--get-response-status (buffer)
  "Get the response status from BUFFER."
  (with-current-buffer buffer
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun aa--get-response-json (buffer)
  "Get the response JSON from BUFFER."
  (with-current-buffer buffer
    (let ((json-object-type 'plist))
      (progn
        (forward-paragraph)
        (let ((json-str
               (decode-coding-string
                (buffer-substring-no-properties (point) (point-max))
                'utf-8)))
          (condition-case nil
              (json-read-from-string json-str)
            (json-readtable-error 'json-read-error)))))))

(defun aa--show-debug-buffer (buffer)
  "Show debug BUFFER if desired."
  (with-current-buffer buffer (when aa-debug-mode (clone-buffer "*aa-error*" 'show))))

(provide 'aero-assistant-openai)
;;; aero-assistant-openai.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("aa-" . "aero/assistant-"))
;; End:

                                        ; LocalWords:  aa

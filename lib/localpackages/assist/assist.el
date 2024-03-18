;;; assist.el --- Aero AI Assistant client  -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2023-2024 Jade Michael Thornton
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.1") (spinner "1.7.4"))
;; Version: 0.9.0
;;
;; This file is not part of GNU Emacs

;;; Commentary:
;;
;; Assist is an Emacs Lisp package that acts as an AI client for Aero Emacs. It leverages various AI
;; models, including GPT-4, Claude and DALL-E to facilitate a broad range of assistant tasks.

;; API Key:
;;
;; GPT and DALL-E usage requires setting `assist-openai-api-key' to your own API key.
;;
;; Claude usage requires setting `assist-anthropic-api-key' to your own API key.

;; Usage:
;;
;; Simply call the `assist-chat' function to start or switch to an Assist session:
;;
;; (assist-chat)
;;
;; If a region is active, `assist' will prefill the input buffer with the content of the
;; region.

;; Using Assist for Git Commit Messages in Magit [Experimental]:
;;
;; The `assist-commit-message' function can add an Aero Assist- generated commit message.
;; This function requires [Magit](https://github.com/magit/magit).
;;
;; Whenever you commit using Magit, calling `assist-commit-message' will automatically
;; generate a commit message based on the staged git changes
;;
;; CAUTION: GPT isn't actually very good at writing commit messages, so consider this feature
;; experimental, probably forever

;; Using Assist to generate Gherkin comments from spec files [Experimental]:
;;
;; Use the `assist-diff-qa-steps'.

;; Enable request debugging
;;
;; To see the output from API requests, toggle on debugging mode with `assist-toggle-debug'.

;;; License:
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

;;; Code:


;; History management

(defvar assist-chat-history nil
  "A list of chat histories by session-id

This is a list of cons cells, where the car is the session-id and the cdr is a list of chat")

(defmacro assist-get-session-id ()
  "Get a new session ID."
  (gensym "assist-session-id"))

(defun assist-chat-add-to-history (session-id item))

(defun assist-chat-get-history (session-id)
  "Get the chat history for SESSION-ID."
  (cdr (assoc session-id assist-chat-history)))

(defun assist-chat-get-history-last-item (session-id))


;; Requests

(defvar assist-model-aliases '((gpt-4 . "gpt-4-0125-preview")
                               (dall-e-3 . "dall-e-3"))
  "A list of model aliases.")

(defvar assist-session-models nil
  "A list of session IDs and the models they're using.

This is a list of cons cells, where the car is the session ID and the cdr is
the model name.")

(defun assist-decode-model-alias (alias)
  "Decode an ALIAS into the actual model name."
  (cdr (assoc alias assist-model-aliases)))

(defun assist-get-session-model (session)
  "Get the model for SESSION."
  (cdr (assoc session assist-session-models)))


;;;;;; OOOOLLLLDDDD

(declare-function gfm-mode "markdown-mode")

(eval-when-compile (require 'subr-x))
(require 'url)
(require 'aero-lib)
(require 'spinner)
(require 'json)
(require 'map)
(require 'markdown-mode)

(defgroup assist nil
  "Aero Assist."
  :prefix "assist-"
  :group 'emacs-ml)

(defcustom assist-openai-api-key nil
  "An OpenAI API key."
  :group 'assist
  :type 'string)

(defcustom assist-max-entries nil
  "Max chat entries to send to remote LLM for context.

Nil means no maximum."
  :group 'assist
  :type 'number)

(defvar assist-debug-mode nil)
(defvar assist--session-name "*Aero Assist*")
(defvar assist--input-buffer-name "*Aero Assist Input*")
(defvar assist--history '())
(defvar assist--busy-p nil)
(defvar assist--spinner nil)

(defconst assist-openai-system-prompt
  "You will act as a brilliant and experienced senior software engineer working in Emacs; you are a helpful assistant and a careful, wise programmer. You do not have feelings and you do not apologize.
The user is a senior software engineer with limited time.
You treat the user's time as precious, but you are not afraid to ask for clarification when needed.
You do not repeat obvious things, including the user's query.
You never apologize for confusions because it would waste their time.
Respond concisely and cite sources for factual claims.
Do not explain code snippets unless asked to do so.
Use Markdown formatting liberally in all messages.
Always show code snippets in markdown blocks with language labels.
Whenever you output updated code for the user, you only show diffs instead of entire snippets unless asked.
When using Python, assume the user is using version 3.9 or newer.
When using Typescript, assume the user is using version 4.8 or newer.")

(defconst assist-commit-git-command
  '("diff" "--cached" "--summary" "-U10" "--no-color")
  "The git command given to Magit to get the commit diff.")

(defconst assist-commit-system-prompt
  "You are acting as a brilliant and experienced senior software engineer. The user will provide the result of running 'git %s'. You will suggest a commit message based on the diff. Do not respond with anything other than the commit message. The following describes guidelines for a proper commit message; you must follow them carefully at all times.

The message MUST NEVER exceed 50 characters.
The message MUST ALWAYS begin with a lower-case letter.
The message MUST be in the imperative mood.
The message must not end with a period, and should not end with any other punctuation.
The message must not begin with a commit type (e.g. \"fix:\", \"feat:\", \"docs:\", etc.)
The message must not include a commit body, you must respond with the commit message only.
The message should not include file names.
The message should avoid using the verb \"to be\".
The message should not use the word \"refactor\".
When a submodule is being updated, the commit message should mention the name of the submodule.
Once again, the message must never ever exceed 50 characters.
")

(defconst assist-gen-qa-system-prompt
  "You are a brilliant and experienced senior software engineer. The user will provide the result of running a git diff, including only changes to test files. You will provide QA testing steps which match the tests which have been added or changed. The result must be in the form of a markdown checkbox list, though the list may be broken into sections for multiple features. The wording should use Gherkin keywords such as 'given', 'when' and 'then'.")

(defvar assist--model "GPT 4")
(defvar assist-commit-model "GPT 3.5")
(defvar assist--model-options
  '("GPT 3.5"
    "GPT 4"
    "DALL-E 3"
    "DALL-E 2"))
(defvar assist--openai-models '("GPT 4" "GPT 3.5" "DALL-E 3" "DALL-E 2"))
(defvar assist--dall-e-models '("dall-e-3" "dall-e-2"))
(defvar assist--gpt-models '("gpt-4-turbo-preview" "gpt-3.5-turbo-1106"))
(defvar assist--model-name-map
  #s(hash-table size 10 test equal data
                ("GPT 3.5" "gpt-3.5-turbo-1106"
                 "GPT 4" "gpt-4-turbo-preview"
                 "DALL-E 3" "dall-e-3"
                 "DALL-E 2" "dall-e-2")))
(defvar assist-dall-e-quality "standard")
(defvar assist-dall-e-style "vivid")

(defun assist-toggle-debug ()
  "Toggle Aero Assist debug mode."
  (interactive)
  (setq assist-debug-mode (not assist-debug-mode))
  (if assist-debug-mode
      (message "Aero Assist debug mode enabled")
    (message "Aero Assist debug mode disabled")))

(defun assist-set-dall-e-quality ()
  "Set DALL-E quality.

The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image"
  (setq assist-dall-e-quality
        (completing-read "DALL-E Quality: " '("standard" "hd")
                         nil nil nil nil
                         assist-dall-e-quality)))

(defun assist-set-dall-e-style ()
  "Set DALL-E style.

Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images"
  (setq assist-dall-e-style
        (completing-read "DALL-E Style: " '("vivid" "natural")
                         nil nil nil nil
                         assist-dall-e-style)))

(defun assist-kill-buffer-hook ()
  "Kill response buffer hook."
  (spinner-stop assist--spinner)
  (setq assist--busy-p nil)
  (setq assist--history '()))

(defmacro assist-without-readonly (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))

(defmacro assist--get-model (model-var)
  "Get the model set for MODEL-VAR."
  (declare (indent 0))
  `(gethash ,model-var assist--model-name-map))


;; API interaction

(defun assist--send-openai (model)
  "Send prompts to OpenAI MODEL."
  (unless assist-openai-api-key (user-error "Must set `assist-openai-api-key'"))
  (let ((single-prompt (member model assist--dall-e-models)))
    (assist--send-openai-request
     model
     (assist--gather-prompts-openai single-prompt)
     (lambda (response)
       (let ((message (assist--register-response response)))
         (assist--display-message message)
         (markdown-display-inline-images)
         (setq assist--busy-p nil)
         (spinner-stop assist--spinner)
         ;; move point to bottom, then scroll to have margin
         (setf (point) (point-max))
         (recenter -15))))))

(defun assist--gen-commit-message-openai (model callback)
  "Generate a commit message and pass it to CALLBACK."
  (let* ((diff-lines (magit-git-lines "diff" "--cached"))
         (changes (string-join diff-lines "\n"))
         (system-prompt (format assist-commit-system-prompt (string-join assist-commit-git-command " ")))
         (message (list (list :role "system" :content system-prompt)
                        (list :role "user" :content changes))))
    (message "Aero Assist is generating a commit message...")
    (assist--send-openai-request
     model message
     (lambda (message)
       (funcall callback (assist--format-commit-message-content message))))))

(defun assist--gen-qa-steps (diff callback)
  "Generate QA steps from DIFF, then call CALLBACK with the result."
  (let* ((model (assist--get-model assist-commit-model))
         (message (list (list :role "system" :content assist-gen-qa-system-prompt)
                        (list :role "user" :content diff))))
    (assist--send-openai-request model message (lambda (message) (funcall callback message)))))

(defun assist--format-commit-message-content (message)
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

(defun assist--send-openai-request (model message callback)
  "Send MESSAGE to OpenAI MODEL and call CALLBACK with the response."
  (unless assist-openai-api-key (user-error "Must set `assist-openai-api-key'"))
  (let ((inhibit-message t)
        (message-log-max nil)
        (url-show-status assist-debug-mode)
        (url-show-headers assist-debug-mode)
        (url-request-method "POST")
        (url-request-extra-headers
         `(("Content-Type" . "application/json")
           ("Authorization" . ,(format "Bearer %s" assist-openai-api-key))))
        (url-request-data (assist--request-data model message)))

    (url-retrieve (assist--openapi-api-url model)
                  (lambda (_)
                    (funcall callback (assist--parse-response-openai model (current-buffer)))
                    (kill-buffer))
                  nil nil nil)))

(defun assist--request-data (model message)
  "Get request data for MODEL with MESSAGE"
  (cond ((member model assist--dall-e-models)
         (assist--request-datassist-dall-e model message))
        ((member model assist--gpt-models)
         (assist--request-datassist-gpt model message))
        (t (user-error "Model %s not supported" model))))

(defun assist--openapi-api-url (model)
  "Get the OpenAI API URL for MODEL."
  (cond ((member model assist--dall-e-models)
         "https://api.openai.com/v1/images/generations")
        ((member model assist--gpt-models)
         "https://api.openai.com/v1/chat/completions")
        (t (user-error "Model %s not supported" model))))

(defun assist--request-datassist-dall-e (model message)
  "Get the request data for a DALL-E MODEL call with MESSAGE."
  (encode-coding-string
   (json-encode `(:model ,model
                  :prompt ,message
                  :n 1 ;; only 1 is supported for dall-e 3
                  :quality ,assist-dall-e-quality
                  :style ,assist-dall-e-style))
   'utf-8))

(defun assist--request-datassist-gpt (model messages)
  "Get the request data for a GPT MODEL call with MESSAGES."
  (encode-coding-string
   ;; https://platform.openai.com/docs/api-reference/chat/create
   (json-encode `(:model ,model
                  :messages [,@messages]
                  :temperature nil
                  :max_tokens nil))
   'utf-8))

(defun assist--gather-prompts-openai (single-prompt)
  "Return a full prompt from chat history, or last prompt if SINGLE-PROMPT."
  (let ((prompts (assist--filter-history-prompts-format-openai
                  #'assist--valid-prompt-p
                  (or (and assist-max-entries
                           (seq-take assist--history assist-max-entries))
                      assist--history))))
    (when (not prompts) (user-error "Prompt history contains nothing to send."))
    (if single-prompt
        (let ((last-prompt (car assist--history)))
          (when (string= "user" (plist-get last-prompt :role))
            (plist-get last-prompt :content)))
      (cons (list :role "system"
                  :content assist-openai-system-prompt)
            ;; Need to reverse so latest comes last
            (nreverse prompts)))))

(defun assist--filter-history-prompts-format-openai (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (assist--format-openai-prompt (car hist))
              (assist--filter-history-prompts-format-openai pred (cdr hist)))
      (assist--filter-history-prompts-format-openai pred (cdr hist)))))

(defun assist--format-openai-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(defun assist--parse-response-openai (model buffer)
  "Parse the Assistant response for GPT in BUFFER for MODEL."
  (cond ((member model assist--dall-e-models)
         (assist--parse-response-openai-dall-e buffer))
        ((member model assist--gpt-models)
         (assist--parse-response-openai-gpt buffer))
        (t (user-error "Model %s not supported" model))))

(defun assist--parse-response-openai-dall-e (buffer)
  "Parse the assistant response for DALL-E in BUFFER."
  (when (buffer-live-p buffer)
    (assist--show-debug-buffer buffer)
    (if-let ((status (assist--get-response-status buffer))
             (response (assist--get-response-json buffer)))
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

(defun assist--parse-response-openai-gpt (buffer)
  "Parse the assistant response for GPT in BUFFER."
  (when (buffer-live-p buffer)
    (assist--show-debug-buffer buffer)
    (if-let ((status (assist--get-response-status buffer))
             (response (assist--get-response-json buffer)))
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

(defun assist--get-response-status (buffer)
  "Get the response status from BUFFER."
  (with-current-buffer buffer
    (buffer-substring (line-beginning-position) (line-end-position))))

(defun assist--get-response-json (buffer)
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

(defun assist--show-debug-buffer (buffer)
  "Show debug BUFFER if desired."
  (with-current-buffer buffer (when assist-debug-mode (clone-buffer "*assist-error*" 'show))))

(defun assist--valid-prompt-p (item)
  "Return t if ITEM is a valid prompt.

A prompt is a valid message which has a role of either user or assistant and contains message
content and no error marker."
  (and (assist--valid-message-p item)
       (not (plist-get item :error))
       (and (or (string= (plist-get item :role) "user")
                (string= (plist-get item :role) "assistant"))
            (not (string-empty-p (plist-get item :content))))))

(defun assist--valid-message-p (item)
  "Return t if ITEM is a valid message.

A valid message is a plist containing either an error and a status or a role and content. Any of
these may be nil and still be a valid message, they need only exist."
  (and item
       (plistp item)
       (or (and (plist-member item :error)
                (plist-member item :status))
           (and (plist-member item :role)
                (plist-member item :content)))))

(defun assist--register-response (response)
  "Add assistant response to history, return prompt alist."
  (let ((prompt (map-merge 'plist '(:role "assistant") response)))
    (push prompt assist--history)
    prompt))

(defun assist--register-user-message (input)
  "Add user message to history, return prompt alist."
  (let ((prompt (list :role "user" :content (string-trim input " \t\n\r"))))
    (push prompt assist--history)
    prompt))

(defun assist--git-diff-spec-files ()
  (let ((default-directory (or (project-root (project-current)) (vc-root-dir) default-directory)))
    (if (zerop (call-process "git" nil nil nil "fetch" "origin" "master:refs/remotes/origin/master"))
        (shell-command-to-string "git diff origin/master...HEAD --summary -U10 --no-color -- '**/specs/**' '*.spec.ts'")
      "Failed to fetch origin/master.")))


;; User input

(defun assist-begin-input (&optional init)
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (when assist--busy-p
    (user-error "BUSY: Waiting for assistant complete its response..."))
  (assist-input-exit)
  (let ((dir (if (window-parameter nil 'window-side) 'bottom 'down))
        (buf (get-buffer-create assist--input-buffer-name)))
    (with-current-buffer buf
      (assist-input-mode)
      (erase-buffer)
      (when init (insert init))
      (call-interactively #'set-mark-command)
      (setf (point) (point-min)))
    (pop-to-buffer buf `((display-buffer-in-direction)
                         (reusable-frames . nil)
                         (direction . ,dir)
                         (dedicated . t)
                         (window-height . 30)))))

(defun assist-try-again ()
  "In the case of an error, try again."
  (interactive)
  (unless assist--history
    (user-error "No assistant history to try again with."))
  (assist-send))

(defun assist-send ()
  "Submit the current prompt to assistant."
  (interactive)
  (let ((model (gethash assist--model assist--model-name-map)))
    (setq assist--busy-p t)
    (spinner-start assist--spinner)
    (assist--send-openai model)))

(defun assist-input-exit ()
  (interactive)
  (when-let ((buf (get-buffer assist--input-buffer-name)))
    (kill-buffer buf)))

(defun assist-input-send ()
  (interactive)
  (when assist--busy-p
    (user-error "BUSY: Waiting for assistant complete its response..."))
  (with-current-buffer assist--input-buffer-name
    (let ((input (buffer-substring-no-properties (point-min) (point-max))))
      (when (string-empty-p input)
        (user-error "No input to send"))
      (assist--display-message (assist--register-user-message input))
      (assist-send)
      (assist-input-exit)
      (pop-to-buffer assist--session-name)
      ;; move point to bottom, then scroll to have margin
      (setf (point) (point-max))
      (recenter -15))))

(defvar assist-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") #'assist-input-send)
    (define-key map (kbd "C-c C-c") #'assist-input-send)
    (define-key map (kbd "C-c C-r") #'assist-try-again)
    (define-key map (kbd "C-c C-k") #'assist-input-exit)
    map))

(define-derived-mode assist-input-mode gfm-mode "Aero Assist Input"
  "Major mode for Aero Assist input mode.

\\<assist-input-mode-map>"
  (setq header-line-format '(" Aero Assist Input  |  C-RET to send, C-c C-k to cancel "))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'assist-input-mode 'insert))
  (markdown-toggle-fontify-code-blocks-natively))


;; Chat init and display

(defun assist--display-message (message)
  "Display the most recent history message."
  (unless (assist--valid-message-p message)
    (error "Message is not valid: %s" message))
  (with-current-buffer assist--session-name
    (assist-without-readonly
      (setf (point) (point-max))
      (let* ((message-content (plist-get message :content))
             (role (plist-get message :role)))
        (unless (bobp) (insert "\n\n"))
        (cond
         ((or (plist-get message :error) (eq role nil))
          (insert "## Assistant [Error]\n\n> Try again with C-c C-r [assist-try-again]\n\n"
                  (or (plist-get message :status)
                      (format (or (and (plist-get message :error)
                                       "Error: unknown error: %s")
                                  (and (eq role nil)
                                       "Error: message has no role: %s")
                                  "Error: invalid message: %s")
                              message))
                  "\n\n\f\n"))

         ((string= role "user")
          (insert "# User\n\n" message-content))

         ((string= role "assistant")
          (insert (assist--format-response message))))))))

(defun assist--format-response (response)
  "Format assistant response for display."
  (let ((content (plist-get response :content))
        (tokens (plist-get response :tokens))
        (revised-prompt (plist-get response :revised_prompt))
        (show-image (plist-get response :show_image))
        (time (plist-get response :time))
        (stop (plist-get response :stop)))
    (concat "## Assistant"
            (when time
              ;; Format time
              (format "\n\n> [%s]" (format-time-string "%Y-%m-%d %H:%M:%S" time)))
            (when tokens
              (concat
               "\n"
               (unless time "\n")
               (format "> %s tokens: %s prompt, %s response"
                       (plist-get tokens :total_tokens)
                       (plist-get tokens :prompt_tokens)
                       (plist-get tokens :completion_tokens)))
              )
            (when revised-prompt
              (concat "\n\n> " revised-prompt))
            "\n\n"
            content
            "\n\n"
            ;; (when show-image (concat "![](" content ")\n\n"))
            (cond
             ((string= stop "length") "> Stop Reason: Token Limit")
             ((string= stop "content_filter") "> Stop Reason: Content Filter Flag")
             (t ""))
            "\f\n")))

(defun assist-clear-history ()
  (interactive)
  (when (y-or-n-p "Clear Aero Assist history forever?")
    (with-current-buffer assist--session-name
      (assist-without-readonly
        (setq assist--history '())
        (insert "\n\n\f\n# HISTORY CLEARED\n\f\n")))))

(defun assist--header-line ()
  "Display header line."
  (format " %s Aero Assist  |  %s"
          (if-let ((spinner (spinner-print assist--spinner)))
              (concat spinner " ")
            " ")
          assist--model))

(defun assist--assert-message-not-error (message)
  "Throw user-error if MESSAGE is empty or has an error."
  (unless message (user-error "Aero Assist commit message error: no response"))
  (when (plist-get message :error)
    (user-error "Aero Assist commit message error: %s" (plist-get message :status)))
  (unless (plist-get message :content)
    (user-error "Aero Assist commit message error: no response content")))

(defun assist--insert-commit-message (message buf)
  "Insert content of MESSAGE at the start of buffer BUF.

MESSAGE should be an assist formatted plist."
  (assist--assert-message-not-error message)
  (let ((content (plist-get message :content)))
    (with-current-buffer buf
      (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
        ;; Only insert if message line is empty
        (insert content)))))

(defun assist--display-qa-steps (message)
  "Display content of MESSAGE in a display buffer."
  (assist--assert-message-not-error message)
  (let ((content (plist-get message :content)))
    (with-current-buffer (get-buffer-create "*assist-diff-qa-steps*")
      (erase-buffer)
      (insert "# Aero Assist QA Steps\n\n" content)))
  (markdown-mode)
  (markdown-toggle-fontify-code-blocks-natively))

(defun assist-set-model ()
  "Prompt user to set the assistant model and verify key if required."
  (interactive)
  (setq assist--model
        (completing-read "Assistant model: " assist--model-options
                         nil nil nil nil
                         assist--model))
  ;; check for keys
  (cond
   ((member assist--model assist--openai-models)
    (unless assist-openai-api-key (user-error "Must set `assist-openai-api-key'")))))

(defvar assist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'assist-begin-input)
    (define-key map (kbd "C-c C-k") #'assist-clear-history)
    map))

(define-derived-mode assist-mode gfm-mode "Aero Assist"
  "Major mode for Aero Assist response mode.

\\<assist-mode-map>"
  (setq buffer-read-only t)
  (setq header-line-format '((:eval (assist--header-line))))
  (setq assist--spinner (spinner-create 'horizontal-breathing-long t))
  (markdown-display-inline-images)
  (markdown-toggle-fontify-code-blocks-natively)
  (add-hook 'kill-buffer-hook #'assist-kill-buffer-hook nil t))

;;;###autoload
(defun assist-chat (&optional init)
  "Switch to or start an Aero Assist chat session.

If region is active, prefill input buffer with the region."
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (let ((buf (get-buffer-create assist--session-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'assist-mode)
        (assist-mode))
      (let ((blank (string-empty-p (buffer-string))))
        (assist-without-readonly
          (switch-to-buffer buf)
          (setf (point) (point-max))
          (when blank (assist-begin-input init)))))))

;;;###autoload
(defun assist-commit-message ()
  "Aero Assist generates a commit message.

Independently of `assist', this function uses the model defined by `assist-commit-model'.

Requires `magit'."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "This function requires `magit'"))
  (unless (git-commit-buffer-message)
    (let ((buf (current-buffer))
          (model (assist--get-model assist-commit-model)))
      ;; TODO count tokens first
      (assist--gen-commit-message-openai model #'assist--insert-commit-message))))

(defun assist-diff-qa-steps ()
  "Create Gherkin-like QA steps from the git diff.

Currently only respects .spec.ts files diffed against a master branch. If the default branch is main or something else, this won't work at the moment."
  (interactive)
  (let ((diff (assist--git-diff-spec-files)))
    (pop-to-buffer (get-buffer-create "*assist-diff-qa-steps*"))
    (erase-buffer)
    (insert "Aero Assist is generating QA steps...\n\n")
    (assist--gen-qa-steps diff #'assist--display-qa-steps)))

(provide 'assist)
;;; assist.el ends here

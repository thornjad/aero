;;; aero-assistant.el --- Aero AI Assistant client  -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2023-2024 Jade Michael Thornton
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.1") (spinner "1.7.4"))
;; URL: https://github.com/thornjad/aero/blob/master/lib/localpackages/aero-assistant
;; Version: 0.1.0
;;
;; This file is not part of GNU Emacs

;;; Commentary:
;;
;; Aero Assistant is an Emacs Lisp package that acts as an AI client for Aero Emacs. It leverages
;; various AI models, including GPT-4, GPT-3.5-turbo, and Davinci, to facilitate a broad range of
;; natural language processing tasks right within your Emacs.

;; OpenAI API Key:
;;
;; GPT requires setting `aero/assistant-openai-api-key' to your own API key

;; Usage:
;;
;; Simply call the `aero/assistant' function to start or switch to an Aero Assistant session:
;;
;; (aero/assistant)
;;
;; If a region is active, `aero/assistant' will prefill the input buffer with the content of the
;; region.

;; Using Aero Assistant for Git Commit Messages in Magit [Experimental]:
;;
;; The `aero/assistant-commit-message' function can add an Aero Assistant- generated commit message.
;; This function requires [Magit](https://github.com/magit/magit).
;;
;; Whenever you commit using Magit, calling `aero/assistant-commit-message' will automatically
;; generate a commit message based on the staged git changes
;;
;; CAUTION: GPT isn't actually very good at writing commit messages, so consider this feature
;; experimental, probably forever

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

(declare-function gfm-mode "markdown-mode")

(eval-when-compile (require 'subr-x))
(require 'url)
(require 'aero-lib)
(require 'spinner)
(require 'json)
(require 'map)
(require 'markdown-mode)

(defgroup aero/assistant nil
  "Aero Assistant."
  :prefix "aa-"
  :group 'emacs-ml)

(defcustom aa-openai-api-key nil
  "An OpenAI API key."
  :group 'aero/assistant
  :type 'string)

(defcustom aa-max-entries nil
  "Max chat entries to send to remote LLM for context.

Nil means no maximum."
  :group 'aero/assistant
  :type 'number)

(defvar aa-debug-mode nil)
(defvar aa--session-name "*Aero Assistant*")
(defvar aa--input-buffer-name "*Aero Assistant Input*")
(defvar aa--history '())
(defvar aa--busy-p nil)
(defvar aa--spinner nil)

(defconst aa-openai-system-prompt
  "You will act as a brilliant and experienced senior software engineer working in Emacs; you are a helpful assistant and a careful, wise programmer.
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

(defconst aa-commit-git-command
  '("diff" "--cached" "--summary" "-U10" "--no-color")
  "The git command given to Magit to get the commit diff.")

(defconst aa-commit-system-prompt
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

(defvar aa--model "GPT 4")
(defvar aa-commit-model "GPT 3.5")
(defvar aa--model-options
  '("GPT 3.5"
    "GPT 4"
    "DALL-E 3"
    "DALL-E 2"))
(defvar aa--openai-models '("GPT 4" "GPT 3.5" "DALL-E 3" "DALL-E 2"))
(defvar aa--dall-e-models '("dall-e-3" "dall-e-2"))
(defvar aa--gpt-models '("gpt-4-1106-preview" "gpt-3.5-turbo-1106"))
(defvar aa--model-name-map
  #s(hash-table size 10 test equal data
                ("GPT 3.5" "gpt-3.5-turbo-1106"
                 "GPT 4" "gpt-4-1106-preview"
                 "DALL-E 3" "dall-e-3"
                 "DALL-E 2" "dall-e-2")))
(defvar aa-dall-e-quality "standard")
(defvar aa-dall-e-style "vivid")

(defun aero/assistant-toggle-debug ()
  "Toggle Aero Assistant debug mode."
  (interactive)
  (setq aa-debug-mode (not aa-debug-mode))
  (if aa-debug-mode
      (message "Aero Assistant debug mode enabled")
    (message "Aero Assistant debug mode disabled")))

(defun aero/assistant-set-dall-e-quality ()
  "Set DALL-E quality.

The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image"
  (setq aa-dall-e-quality
        (completing-read "DALL-E Quality: " '("standard" "hd")
                         nil nil nil nil
                         aa-dall-e-quality)))

(defun aero/assistant-set-dall-e-style ()
  "Set DALL-E style.

Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images"
  (setq aa-dall-e-style
        (completing-read "DALL-E Style: " '("vivid" "natural")
                         nil nil nil nil
                         aa-dall-e-style)))

(defun aa-kill-buffer-hook ()
  "Kill response buffer hook."
  (spinner-stop aa--spinner)
  (setq aa--busy-p nil)
  (setq aa--history '()))

(defmacro aa-without-readonly (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))


;; API interaction


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
                  :content aa-openai-system-prompt)
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

(defun aa--valid-prompt-p (item)
  "Return t if ITEM is a valid prompt.

A prompt is a valid message which has a role of either user or assistant and contains message
content and no error marker."
  (and (aa--valid-message-p item)
       (not (plist-get item :error))
       (and (or (string= (plist-get item :role) "user")
                (string= (plist-get item :role) "assistant"))
            (not (string-empty-p (plist-get item :content))))))

(defun aa--valid-message-p (item)
  "Return t if ITEM is a valid message.

A valid message is a plist containing either an error and a status or a role and content. Any of
these may be nil and still be a valid message, they need only exist."
  (and item
       (plistp item)
       (or (and (plist-member item :error)
                (plist-member item :status))
           (and (plist-member item :role)
                (plist-member item :content)))))

(defun aa--register-response (response)
  "Add Assistant response to history, return prompt alist."
  (let ((prompt (map-merge 'plist '(:role "assistant") response)))
    (push prompt aa--history)
    prompt))

(defun aa--register-user-message (input)
  "Add user message to history, return prompt alist."
  (let ((prompt (list :role "user" :content (string-trim input " \t\n\r"))))
    (push prompt aa--history)
    prompt))


;; User input

(defun aa-begin-input (&optional init)
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (when aa--busy-p
    (user-error "BUSY: Waiting for Assistant complete its response..."))
  (aa-input-exit)
  (let ((dir (if (window-parameter nil 'window-side) 'bottom 'down))
        (buf (get-buffer-create aa--input-buffer-name)))
    (with-current-buffer buf
      (aa-input-mode)
      (erase-buffer)
      (when init (insert init))
      (call-interactively #'set-mark-command)
      (setf (point) (point-min)))
    (pop-to-buffer buf `((display-buffer-in-direction)
                         (reusable-frames . nil)
                         (direction . ,dir)
                         (dedicated . t)
                         (window-height . 30)))))

(defun aa-try-again ()
  "In the case of an error, try again."
  (interactive)
  (unless aa--history
    (user-error "No Assistant history to try again with."))
  (aa-send))

(defun aa-send ()
  "Submit the current prompt to Assistant."
  (interactive)
  (let ((model (gethash aa--model aa--model-name-map)))
    (setq aa--busy-p t)
    (spinner-start aa--spinner)
    (aa--send-openai model)))

(defun aa-input-exit ()
  (interactive)
  (when-let ((buf (get-buffer aa--input-buffer-name)))
    (kill-buffer buf)))

(defun aa-input-send ()
  (interactive)
  (when aa--busy-p
    (user-error "BUSY: Waiting for Assistant complete its response..."))
  (with-current-buffer aa--input-buffer-name
    (let ((input (buffer-substring-no-properties (point-min) (point-max))))
      (when (string-empty-p input)
        (user-error "No input to send"))
      (aa--display-message (aa--register-user-message input))
      (aa-send)
      (aa-input-exit)
      (pop-to-buffer aa--session-name))))

(defvar aa-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-return>") #'aa-input-send)
    (define-key map (kbd "C-c C-c") #'aa-input-send)
    (define-key map (kbd "C-c C-r") #'aa-try-again)
    (define-key map (kbd "C-c C-k") #'aa-input-exit)
    map))

(define-derived-mode aa-input-mode gfm-mode "Aero Assistant Input"
  "Major mode for Aero Assistant input mode.

\\<aa-input-mode-map>"
  (setq header-line-format '(" Aero Assistant Input  |  C-RET to send, C-c C-k to cancel "))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'aa-input-mode 'insert))
  (markdown-toggle-fontify-code-blocks-natively))


;; Chat init and display

(defun aa--display-message (message)
  "Display the most recent history message."
  (unless (aa--valid-message-p message)
    (error "Message is not valid: %s" message))
  (with-current-buffer aa--session-name
    (aa-without-readonly
     (setf (point) (point-max))
     (let* ((message-content (plist-get message :content))
            (role (plist-get message :role)))
       (unless (bobp) (insert "\n\n"))
       (cond
        ((or (plist-get message :error) (eq role nil))
         (insert "## Assistant [Error]\n\n> Try again with C-c C-r [aero/assistant-try-again]\n\n"
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
         (insert (aa--format-response message))))

       ;; move point to bottom
       (setf (point) (point-max))))))

(defun aa--format-response (response)
  "Format Assistant response for display."
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

(defun aa-clear-history ()
  (interactive)
  (when (y-or-n-p "Clear Aero Assistant history forever?")
    (with-current-buffer aa--session-name
      (aa-without-readonly
       (setq aa--history '())
       (insert "\n\n\f\n# HISTORY CLEARED\n\f\n")))))

(defun aa--header-line ()
  "Display header line."
  (format " %s Aero Assistant  |  %s"
          (if-let ((spinner (spinner-print aa--spinner)))
              (concat spinner " ")
            " ")
          aa--model))

(defun aa-set-model ()
  "Prompt user to set the Assistant model and verify key if required."
  (interactive)
  (setq aa--model
        (completing-read "Assistant model: " aa--model-options
                         nil nil nil nil
                         aa--model))
  ;; check for keys
  (cond
   ((member aa--model aa--openai-models)
    (unless aa-openai-api-key (user-error "Must set `aa-openai-api-key'")))))

(defvar aa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'aa-begin-input)
    (define-key map (kbd "C-c C-k") #'aa-clear-history)
    map))

(define-derived-mode aa-mode gfm-mode "Aero Assistant"
  "Major mode for Aero Assistant response mode.

\\<aa-mode-map>"
  (setq buffer-read-only t)
  (setq header-line-format '((:eval (aa--header-line))))
  (setq aa--spinner (spinner-create 'horizontal-breathing-long t))
  (markdown-display-inline-images)
  (markdown-toggle-fontify-code-blocks-natively)
  (add-hook 'kill-buffer-hook #'aa-kill-buffer-hook nil t))

;;;###autoload
(defun aero/assistant (&optional init)
  "Switch to or start an Aero Assistant session.

If region is active, prefill input buffer with the region."
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (let ((buf (get-buffer-create aa--session-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'aa-mode)
        (aa-mode))
      (let ((blank (string-empty-p (buffer-string))))
        (aa-without-readonly
         (switch-to-buffer buf)
         (setf (point) (point-max))
         (when blank (aa-begin-input init)))))))

;;;###autoload
(defun aero/assistant-commit-message ()
  "Aero Assistant generates a commit message.

Independently of `aero/assistant', this function uses the model defined by `aero/assistant-commit-model'.

Requires `magit'."
  (interactive)
  (unless (require 'magit nil t)
    (user-error "This function requires `magit'"))
  (unless (git-commit-buffer-message)
    (let ((buf (current-buffer))
          (model (gethash aa-commit-model aa--model-name-map)))
      (require 'aero-assistant-openai)
      ;; TODO count tokens first
      (aa--gen-commit-message-openai
       model
       (lambda (message)
         (unless message (user-error "Aero Assistant commit message error: no response"))
         (when (plist-get message :error)
           (user-error "Aero Assistant commit message error: %s" (plist-get message :status)))
         (let ((content (plist-get message :content)))
           (unless content (user-error "Aero Assistant commit message error: no response content"))
           (with-current-buffer buf
             (when (string-match-p "\\`\\s-*$" (thing-at-point 'line))
               ;; Only insert if message line is empty
               (insert content)))))))))

(provide 'aero-assistant)
;;; aero-assistant.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("aa-" . "aero/assistant-"))
;; End:

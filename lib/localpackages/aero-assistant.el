;;; aero-assistant.el --- Aero AI Assistant client  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Jade Michael Thornton
;; Package-Requires: ((emacs "27.1") (markdown-mode "2.1") (spinner "1.7.4"))
;; Package-Version: 0.1.0
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

(declare-function markdown-mode "markdown-mode")
(declare-function pulse-momentary-highlight-region "pulse")

(eval-when-compile (require 'subr-x))
(require 'url)
(require 'aero-lib)
(require 'spinner)
(require 'json)
(require 'map)
(require 'markdown-mode)

;;; Code:

(defgroup aero/assistant nil
  "Aero Assistant."
  :prefix "aero/assistant-"
  :group 'emacs-ml)

(defcustom aero/assistant-openai-api-key nil
  "An OpenAI API key."
  :group 'aero/assistant
  :type 'string)

(defcustom aero/assistant-max-entries nil
  "Max chat entries to send to remote LLM for context.

Nil means no maximum."
  :group 'aero/assistant
  :type 'number)

(defvar aero/assistant-debug-mode nil)
(defvar aero/assistant--session-name "*Aero Assistant*")
(defvar aero/assistant--input-buffer-name "*Aero Assistant Input*")
(defvar aero/assistant--history '())
(defvar aero/assistant--busy-p nil)
(defvar aero/assistant--spinner nil)

(defvar aero/assistant--model "gpt-3.5-turbo")
(defvar aero/assistant--model-options
  '("gpt-3.5-turbo"
    ;; "gpt-4" ; on API wait list

    ))

(defun aero/assistant-kill-buffer-hook ()
  "Kill response buffer hook."
  (spinner-stop aero/assistant--spinner)
  (setq aero/assistant--busy-p nil)
  (setq aero/assistant--history '()))

(defmacro aero/assistant-without-readonly (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))


;; API

(defun aero/assistant-send ()
  "Submit the current prompt to Assistant."
  (interactive)
  (unless aero/assistant-openai-api-key
    (user-error "Must set `aero/assistant-openai-api-key'"))
  (let* ((prompt (aero/assistant--gather-prompts))
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
                            (json-encode `(:model aero/assistant--model
                                           :messages [,@prompt]
                                           :temperature nil
                                           :max_tokens nil))
                            'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (let ((message (aero/assistant--register-response
                                    (aero/assistant--parse-response (current-buffer)))))
                      (aero/assistant--display-message message)
                      (setq aero/assistant--busy-p nil)
                      (spinner-stop aero/assistant--spinner)
                      (kill-buffer)))
                  nil (not aero/assistant-debug-mode) nil)))

(defun aero/assistant--gather-prompts ()
  "Return a full prompt from chat history, prepended with a system prompt.

GPT-3 does not always respect the system prompt, though GPT-4 should be better at this."
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

(defun aero/assistant--valid-prompt-p (item)
  "Return t if ITEM is a valid prompt.

A prompt is a valid message which has a role of either user or assistant and contains message
content and no error marker."
  (and (aero/assistant--valid-message-p item)
       (not (plist-get item :error))
       (and (or (string= (plist-get item :role) "user")
                (string= (plist-get item :role) "assistant"))
            (not (string-empty-p (plist-get item :content))))))

(defun aero/assistant--valid-message-p (item)
  "Return t if ITEM is a valid message.

A valid message is a plist containing either an error and a status or a role and content. Any of
these may be nil and still be a valid message, they need only exist."
  (and item
       (plistp item)
       (or (and (plist-member item :error)
                (plist-member item :status))
           (and (plist-member item :role)
                (plist-member item :content)))))

(defun aero/assistant--filter-history-prompts-format (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (aero/assistant--format-prompt (car hist))
              (aero/assistant--filter-history-prompts-format pred (cdr hist)))
      (aero/assistant--filter-history-prompts-format pred (cdr hist)))))

(defun aero/assistant--format-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(defun aero/assistant--register-response (response)
  "Add Assistant response to history, return prompt alist."
  (let ((prompt (map-merge 'plist '(:role "assistant") response)))
    (push prompt aero/assistant--history)
    prompt))

(defun aero/assistant--register-user-message (input)
  "Add user message to history, return prompt alist."
  (let ((prompt (list :role "user" :content (string-trim input " \t\n\r"))))
    (push prompt aero/assistant--history)
    prompt)
  )

(defun aero/assistant--parse-response (buffer)
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


;; User input

(defun aero/assistant-begin-input (&optional init)
  (interactive)
  (when aero/assistant--busy-p
    (user-error "BUSY: Waiting for Assistant complete its response..."))
  (aero/assistant-input-exit)
  (let ((dir (if (window-parameter nil 'window-side) 'bottom 'down))
        (buf (get-buffer-create aero/assistant--input-buffer-name)))
    (with-current-buffer buf
      (aero/assistant-input-mode)
      (erase-buffer)
      (when init (insert init))
      (call-interactively #'set-mark-command)
      (setf (point) (point-min)))
    (pop-to-buffer buf `((display-buffer-in-direction)
                         (reusable-frames . nil)
                         (direction . ,dir)
                         (dedicated . t)
                         (window-height . 30)))))

(defun aero/assistant-input-exit ()
  (interactive)
  (when-let ((buf (get-buffer aero/assistant--input-buffer-name)))
    (kill-buffer buf)))

(defun aero/assistant-input-send ()
  (interactive)
  (when aero/assistant--busy-p
    (user-error "BUSY: Waiting for Assistant complete its response..."))
  (with-current-buffer aero/assistant--input-buffer-name
    (let ((input (buffer-substring-no-properties (point-min) (point-max))))
      (when (string-empty-p input)
        (user-error "No input to send"))
      (aero/assistant--send-input input)
      (aero/assistant-input-exit)
      (pop-to-buffer aero/assistant--session-name))))

(defun aero/assistant--send-input (input)
  (let ((message (aero/assistant--register-user-message input) ))
    (aero/assistant--display-message message)
    (setq aero/assistant--busy-p t)
    (spinner-start aero/assistant--spinner)
    (aero/assistant-send)))

(defvar aero/assistant-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'aero/assistant-input-send)
    (define-key map (kbd "C-c C-c") #'aero/assistant-input-send)
    (define-key map (kbd "C-c C-k") #'aero/assistant-input-exit)
    map))

(define-derived-mode aero/assistant-input-mode markdown-mode "Aero Assistant Input"
  "Major mode for Aero Assistant input mode.

\\<aero/assistant-input-mode-map>"
  (setq header-line-format '(" Aero Assistant Input  |  C-RET to send, C-c C-k to cancel "))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'aero/assistant-input-mode 'insert)))


;; Chat display

(defun aero/assistant--display-message (message)
  "Display the most recent history message."
  (unless (aero/assistant--valid-message-p message)
    (error "Message is not valid: %s" message))
  (with-current-buffer aero/assistant--session-name
    (aero/assistant-without-readonly
      (setf (point) (point-max))
      (let* ((message-content (plist-get message :content))
             (role (plist-get message :role)))
        (unless (bobp) (insert "\n\n"))
        (cond
         ((or (plist-get message :error) (eq role nil))
          (insert "## Assistant [Error]\n\n"
                  (or (plist-get message :status)
                      (format (or (and (plist-get message :error)
                                       "Error: unknown error: %s")
                                  (and (eq role nil)
                                       "Error: message has no role: %s")
                                  "Error: invalid message: %s")
                              message))
                  "\n\f\n"))

         ((string= role "user")
          (insert "# User\n\n" message-content))

         ((string= role "assistant")
          (insert (aero/assistant--format-response message))))

        ;; move point to bottom
        (setf (point) (point-max))))))

(defun aero/assistant--format-response (response)
  "Format Assistant response for display."
  (let ((content (plist-get response :content))
        ;; (status (plist-get response :status))
        (tokens (plist-get response :tokens))
        ;; (time (plist-get response :time))
        (stop (plist-get response :stop)))
    (concat "## Assistant "
            ;; Tokens
            (format "— (%s tokens: %s prompt, %s response)"
                    (plist-get tokens :total_tokens)
                    (plist-get tokens :prompt_tokens)
                    (plist-get tokens :completion_tokens))
            "\n\n" content "\n\n"
            (cond
             ((string= stop "length") "> Stop Reason: Token Limit")
             ((string= stop "content_filter") "> Stop Reason: Content Filter Flag")
             (t ""))
            "\f\n")))

(defun aero/assistant-clear-history ()
  (interactive)
  (when (y-or-n-p "Clear Aero Assistant history forever?")
    (with-current-buffer aero/assistant--session-name
      (aero/assistant-without-readonly
        (setq aero/assistant--history '())
        (insert "\n\n\f\n# HISTORY CLEARED\n\f\n")))))

(defun aero/assistant--header-line ()
  "Display header line."
  (format " %s Aero Assistant  |  Using model: %s"
          (if-let ((spinner (spinner-print aero/assistant--spinner)))
              (concat spinner " ")
            " ")
          aero/assistant--model))

(defvar aero/assistant-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'aero/assistant-begin-input)
    (define-key map (kbd "C-c C-k") #'aero/assistant-clear-history)
    map))

(define-derived-mode aero/assistant-mode markdown-mode "Aero Assistant"
  "Major mode for Aero Assistant response mode.

\\<aero/assistant-mode-map>"
  (setq buffer-read-only t)
  (setq header-line-format '((:eval (aero/assistant--header-line))))
  (setq aero/assistant--spinner (spinner-create 'horizontal-breathing-long t))
  (add-hook 'kill-buffer-hook #'aero/assistant-kill-buffer-hook nil t))

;;;###autoload
(defun aero/assistant (&optional init)
  "Switch to or start an Aero Assistant session.

If region is active, prefill input buffer with the region."
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (unless aero/assistant-openai-api-key
    (user-error "Must set `aero/assistant-openai-api-key'"))
  (let ((buf (get-buffer-create aero/assistant--session-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'aero/assistant-mode)
        (aero/assistant-mode))
      (let ((blank (string-empty-p (buffer-string))))
        (aero/assistant-without-readonly
          (when blank (insert "> Use the window below to input your prompt, then C-RET to send. "))
          (pop-to-buffer buf)
          (setf (point) (point-max))
          (when blank (aero/assistant-begin-input init)))))))

(provide 'aero-assistant)

;;; aero-assistant.el ends here

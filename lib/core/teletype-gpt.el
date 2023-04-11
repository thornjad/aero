;;; teletype-gpt.el --- Aero GPT client  -*- lexical-binding: t; -*-
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
;; A simple markdown-based GPT client for Aero Emacs.
;;
;; Requires `teletype-gpt-openai-api-key' to be set, doing so in an `init.local.el' is the right
;; place to do this in Aero.
;;
;; API Reference: https://platform.openai.com/docs/guides/chat
;;
;; TODO new session option, clear history and buffer
;; TODO record running token history
;; TODO better initial view

(declare-function markdown-mode "markdown-mode")
(declare-function pulse-momentary-highlight-region "pulse")

(eval-when-compile (require 'subr-x))
(require 'url)
(require 'spinner)
(require 'json)
(require 'map)
(require 'markdown-mode)
(require 'aero-lib)

;;; Code:

(defgroup teletype-gpt nil
  "Aero Teletype GPT."
  :prefix "teletype-gpt-"
  :group 'emacs-ml)

(defcustom teletype-gpt-openai-api-key nil
  "An OpenAI API key."
  :group 'teletype-gpt
  :type 'string)

(defface teletype-gpt-system-ui
  '((t :inherit font-lock-builtin-face))
  "Face for Teletype GPT UI elements."
  :group 'teletype-gpt)

(defface teletype-gpt-tip
  '((t :inherit font-lock-comment-face))
  "Face for Teletype GPT tips."
  :group 'teletype-gpt)

(defface teletype-gpt-info
  '((t :inherit font-lock-comment-face :height 0.8))
  "Face for GPT info like tokens."
  :group 'teletype-gpt)

(defface teletype-gpt-error
  '((t :inherit error))
  "Face for GPT errors."
  :group 'teletype-gpt)

(defvar teletype-gpt--debug-mode t)
(defvar teletype-gpt--session-name "*Teletype GPT*")
(defvar teletype-gpt--input-buffer-name "*Teletype GPT Input*")
(defvar teletype-gpt--token-history '())
(defvar teletype-gpt--history '())
(defvar teletype-gpt--response-buffer nil)
(defvar teletype-gpt--busy-p nil)
(defvar teletype-gpt--spinner nil)

(defun teletype-gpt-kill-buffer-hook ()
  "Kill response buffer hook."
  (spinner-stop teletype-gpt--spinner)
  (setq teletype-gpt--busy-p nil)
  (setq teletype-gpt--history '())
  (setq teletype-gpt--token-history '())
  (setq teletype-gpt--response-buffer nil))


;; API

(defun teletype-gpt-send ()
  "Submit the current prompt to GPT."
  (interactive)
  (unless teletype-gpt-openai-api-key
    (user-error "Must set `teletype-gpt-openai-api-key'"))
  (let* ((prompt (teletype-gpt--gather-prompts))
         (inhibit-message t)
         (message-log-max nil)
         (url-show-status teletype-gpt--debug-mode)
         (url-show-headers teletype-gpt--debug-mode)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " teletype-gpt-openai-api-key))))
         (url-request-data (encode-coding-string
                            ;; https://platform.openai.com/docs/api-reference/chat/create
                            (json-encode `(:model "gpt-3.5-turbo"
                                           :messages [,@prompt]
                                           :temperature nil
                                           :max_tokens nil))
                            'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (let ((message (teletype-gpt--register-response
                                    (teletype-gpt--parse-response (current-buffer)))))
                      (teletype-gpt--display-message message)
                      (setq teletype-gpt--busy-p nil)
                      (spinner-stop teletype-gpt--spinner)
                      (kill-buffer)))
                  nil (not teletype-gpt--debug-mode) nil)))

(defun teletype-gpt--gather-prompts ()
  "Return a full prompt from chat history, prepended with a system prompt.

GPT-3 does not always respect the system prompt, though GPT-4 should be better at this."
  (let ((prompts (teletype-gpt--filter-history-prompts-format
                  #'teletype-gpt--valid-prompt-p
                  (seq-take teletype-gpt--history 10)))) ; number is max entries to send
    (when (not prompts)
      (user-error "Prompt history contains nothing to send."))
    (cons (list :role "system"
                :content (format "You are a large language model living in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely. Use Github-flavored Markdown formatting in all messages. Current date: %s" (format-time-string "%Y-%m-%d")))
          ;; Need to reverse so latest comes last
          (nreverse prompts))))

(defun teletype-gpt--valid-prompt-p (item)
  "Return t if ITEM is a valid prompt.

A prompt is a valid message which has a role of either user or assistant and contains message
content and no error marker."
  (and (teletype-gpt--valid-message-p item)
       (not (plist-get item :error))
       (and (or (string= (plist-get item :role) "user")
                (string= (plist-get item :role) "assistant"))
            (not (string-empty-p (plist-get item :content))))))

(defun teletype-gpt--valid-message-p (item)
  "Return t if ITEM is a valid message.

A valid message is a plist containing either an error and a status or a role and content. Any of
these may be nil and still be a valid message, they need only exist."
  (and item
       (plistp item)
       (or (and (plist-member item :error)
                (plist-member item :status))
           (and (plist-member item :role)
                (plist-member item :content)))))

(defun teletype-gpt--filter-history-prompts-format (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (teletype-gpt--format-prompt (car hist))
              (teletype-gpt--filter-history-prompts-format pred (cdr hist)))
      (teletype-gpt--filter-history-prompts-format pred (cdr hist)))))

(defun teletype-gpt--format-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(defun teletype-gpt--register-response (response)
  "Add GPT response to history, return prompt alist."
  (let ((prompt (map-merge 'plist '(:role "assistant") response)))
    (push prompt teletype-gpt--history)
    prompt))

(defun teletype-gpt--register-user-message (input)
  "Add user message to history, return prompt alist."
  (let ((prompt (list :role "user" :content (string-trim input " \t\n\r"))))
    (push prompt teletype-gpt--history)
    prompt)
  )

(defun teletype-gpt--parse-response (buffer)
  "Parse the GPT response in URL BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when teletype-gpt--debug-mode (clone-buffer "*teletype-gpt-error*" 'show))
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

(defun teletype-gpt-begin-input ()
  (interactive)
  (when teletype-gpt--busy-p
    (user-error "BUSY: Waiting for GPT complete its response..."))
  (teletype-gpt-input--exit)
  (let ((dir (if (window-parameter nil 'window-side) 'bottom 'down))
        (buf (get-buffer-create teletype-gpt--input-buffer-name)))
    (with-current-buffer buf
      (teletype-gpt-input-mode)
      (erase-buffer)
      (call-interactively #'set-mark-command)
      (setf (point) (point-min)))
    (pop-to-buffer buf `((display-buffer-in-direction)
                         (direction . ,dir)
                         (dedicated . t)
                         (window-height . fit-window-to-buffer)))))

(defun teletype-gpt-input--exit ()
  (when-let ((buf (get-buffer teletype-gpt--input-buffer-name)))
    (kill-buffer buf)))

(defun teletype-gpt-input-send ()
  (interactive)
  (when teletype-gpt--busy-p
    (user-error "BUSY: Waiting for GPT complete its response..."))
  (with-current-buffer teletype-gpt--input-buffer-name
    (let ((input (buffer-substring-no-properties (point-min) (point-max))))
      (when (string-empty-p input)
        (user-error "No input to send"))
      (teletype-gpt--send-input input)
      (teletype-gpt-input--exit))))

(defun teletype-gpt--send-input (input)
  (let ((message (teletype-gpt--register-user-message input) ))
    (teletype-gpt--display-message message)
    (setq teletype-gpt--busy-p t)
    (spinner-start teletype-gpt--spinner)
    (teletype-gpt-send)))

(defun teletype-gpt-input--post-command ()
  "Resize window after input."
  (let ((max-lines (line-number-at-pos (point-max))))
    (fit-window-to-buffer)
    (enlarge-window (- max-lines (window-text-height)))))

(defvar teletype-gpt-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'teletype-gpt-input-send)
    map))

(define-derived-mode teletype-gpt-input-mode markdown-mode "TeletypeGPT Input"
  "Major mode for Aero Teletype GPT input mode.

\\<teletype-gpt-input-mode-map>"
  (add-hook 'post-command-hook #'teletype-gpt-input--post-command nil t)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'teletype-gpt-input-mode 'insert)))


;; Chat display

(defun teletype-gpt--insert-tip ()
  (aero/without-readonly
    (insert (propertize "Press C-return to begin a prompt.\n" 'face 'teletype-gpt-tip))))

(defun teletype-gpt--display-message (message)
  "Display the most recent history message."
  (unless (teletype-gpt--valid-message-p message)
    (error "Message is not valid: %s" message))
  (with-current-buffer teletype-gpt--session-name
    (aero/without-readonly
      (setf (point) (point-max))
      (let* ((message-content (plist-get message :content))
             (role (plist-get message :role)))
        (unless (bobp) (insert "\n\n"))
        (cond
         ((or (plist-get message :error) (eq role nil))
          (insert "# GPT Assistant [Error]\n\n"
                  (propertize (or (plist-get message :status)
                                  (format (or (and (plist-get message :error)
                                                   "Error: unknown error: %s")
                                              (and (eq role nil)
                                                   "Error: message has no role: %s")
                                              "Error: invalid message: %s")
                                          message))
                              'face 'teletype-gpt-error)
                  "\n\f\n"))

         ((string= role "user")
          (insert "# User\n\n" message-content))

         ((string= role "assistant")
          (insert (teletype-gpt--format-response message))))

        ;; move point to bottom
        (setf (point) (point-max))))))

(defun teletype-gpt--format-response (response)
  "Format GPT response for display."
  (let ((content (plist-get response :content))
        ;; (status (plist-get response :status))
        (tokens (plist-get response :tokens))
        ;; (time (plist-get response :time))
        (stop (plist-get response :stop)))
    (concat "# GPT Assistant "
            ;; Tokens
            (propertize (format "Tokens: %s (%s prompt, %s response)"
                                (plist-get tokens :total_tokens)
                                (plist-get tokens :prompt_tokens)
                                (plist-get tokens :completion_tokens))
                        'face 'teletype-gpt-info)
            "\n\n" content "\n\n"
            (cond
             ((string= stop "length") "Stop Reason: Token Limit")
             ((string= stop "content_filter") "Stop Reason: Content Filter Flag")
             (t ""))
            "\f\n")))

(defun teletype-gpt--header-line ()
  "Display header line."
  (format " %s %s history — %s tokens (%s prompt, %s response)"
          (if-let ((spinner (spinner-print teletype-gpt--spinner)))
              (concat spinner " ")
            " ")
          (length teletype-gpt--history)
          (plist-get teletype-gpt--token-history :total)
          (plist-get teletype-gpt--token-history :prompt)
          (plist-get teletype-gpt--token-history :response)))

(defvar teletype-gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'teletype-gpt-begin-input)
    map))

(define-derived-mode teletype-gpt-mode markdown-mode "TeletypeGPT"
  "Major mode for Aero Teletype GPT response mode.

\\<teletype-gpt-mode-map>"
  (setq buffer-read-only t)
  (setq header-line-format '((:eval (teletype-gpt--header-line))
                             " | C-RET to input a prompt"))
  (setq teletype-gpt--spinner (spinner-create 'horizontal-breathing-long t))
  (add-hook 'kill-buffer-hook #'teletype-gpt-kill-buffer-hook nil t))

;;;###autoload
(defun teletype-gpt ()
  "Switch to or start a Teletype GPT session."
  (interactive)
  (unless teletype-gpt-openai-api-key
    (user-error "Must set `teletype-gpt-openai-api-key'"))
  (let ((buf (get-buffer-create teletype-gpt--session-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'teletype-gpt-mode)
        (teletype-gpt-mode))
      (when (string-empty-p (buffer-string)) (teletype-gpt--insert-tip))
      (pop-to-buffer buf)
      (setf (point) (point-max)))))

(provide 'teletype-gpt)

;;; teletype-gpt.el ends here

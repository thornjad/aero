;;; tele-gpt.el --- TeleGPT client  -*- lexical-binding: t; -*-
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
;; A simple markdown-based GPT client for Emacs.
;;
;; Requires `tele-gpt-openai-api-key' to be set
;;
;; API Reference: https://platform.openai.com/docs/guides/chat

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

(defgroup tele-gpt nil
  "TeleGPT."
  :prefix "tele-gpt-"
  :group 'emacs-ml)

(defcustom tele-gpt-openai-api-key nil
  "An OpenAI API key."
  :group 'tele-gpt
  :type 'string)

(defcustom tele-gpt-max-entries nil
  "Max chat entries to send to GPT for context.

Nil means no maximum."
  :group 'tele-gpt
  :type 'number)

(defvar tele-gpt-debug-mode nil)
(defvar tele-gpt--session-name "*TeleGPT*")
(defvar tele-gpt--input-buffer-name "*TeleGPT Input*")
(defvar tele-gpt--history '())
(defvar tele-gpt--busy-p nil)
(defvar tele-gpt--spinner nil)

(defun tele-gpt-kill-buffer-hook ()
  "Kill response buffer hook."
  (spinner-stop tele-gpt--spinner)
  (setq tele-gpt--busy-p nil)
  (setq tele-gpt--history '()))

(defmacro tele-gpt-without-readonly (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))


;; API

(defun tele-gpt-send ()
  "Submit the current prompt to GPT."
  (interactive)
  (unless tele-gpt-openai-api-key
    (user-error "Must set `tele-gpt-openai-api-key'"))
  (let* ((prompt (tele-gpt--gather-prompts))
         (inhibit-message t)
         (message-log-max nil)
         (url-show-status tele-gpt-debug-mode)
         (url-show-headers tele-gpt-debug-mode)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " tele-gpt-openai-api-key))))
         (url-request-data (encode-coding-string
                            ;; https://platform.openai.com/docs/api-reference/chat/create
                            (json-encode `(:model "gpt-3.5-turbo"
                                           :messages [,@prompt]
                                           :temperature nil
                                           :max_tokens nil))
                            'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (let ((message (tele-gpt--register-response
                                    (tele-gpt--parse-response (current-buffer)))))
                      (tele-gpt--display-message message)
                      (setq tele-gpt--busy-p nil)
                      (spinner-stop tele-gpt--spinner)
                      (kill-buffer)))
                  nil (not tele-gpt-debug-mode) nil)))

(defun tele-gpt--gather-prompts ()
  "Return a full prompt from chat history, prepended with a system prompt.

GPT-3 does not always respect the system prompt, though GPT-4 should be better at this."
  (let ((prompts (tele-gpt--filter-history-prompts-format
                  #'tele-gpt--valid-prompt-p
                  (or (and tele-gpt-max-entries
                           (seq-take tele-gpt--history tele-gpt-max-entries))
                      tele-gpt--history))))
    (when (not prompts)
      (user-error "Prompt history contains nothing to send."))
    (cons (list :role "system"
                :content (format "You are a large language model living in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely. Use Markdown formatting in all messages. Current date: %s" (format-time-string "%Y-%m-%d")))
          ;; Need to reverse so latest comes last
          (nreverse prompts))))

(defun tele-gpt--valid-prompt-p (item)
  "Return t if ITEM is a valid prompt.

A prompt is a valid message which has a role of either user or assistant and contains message
content and no error marker."
  (and (tele-gpt--valid-message-p item)
       (not (plist-get item :error))
       (and (or (string= (plist-get item :role) "user")
                (string= (plist-get item :role) "assistant"))
            (not (string-empty-p (plist-get item :content))))))

(defun tele-gpt--valid-message-p (item)
  "Return t if ITEM is a valid message.

A valid message is a plist containing either an error and a status or a role and content. Any of
these may be nil and still be a valid message, they need only exist."
  (and item
       (plistp item)
       (or (and (plist-member item :error)
                (plist-member item :status))
           (and (plist-member item :role)
                (plist-member item :content)))))

(defun tele-gpt--filter-history-prompts-format (pred hist)
  "Filter HIST alist for prompts."
  (when hist
    (if (funcall pred (car hist))
        (cons (tele-gpt--format-prompt (car hist))
              (tele-gpt--filter-history-prompts-format pred (cdr hist)))
      (tele-gpt--filter-history-prompts-format pred (cdr hist)))))

(defun tele-gpt--format-prompt (prompt)
  "Format PROMPT using only keys allowed by the API."
  (list :role (plist-get prompt :role)
        :content (plist-get prompt :content)))

(defun tele-gpt--register-response (response)
  "Add GPT response to history, return prompt alist."
  (let ((prompt (map-merge 'plist '(:role "assistant") response)))
    (push prompt tele-gpt--history)
    prompt))

(defun tele-gpt--register-user-message (input)
  "Add user message to history, return prompt alist."
  (let ((prompt (list :role "user" :content (string-trim input " \t\n\r"))))
    (push prompt tele-gpt--history)
    prompt)
  )

(defun tele-gpt--parse-response (buffer)
  "Parse the GPT response in URL BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when tele-gpt-debug-mode (clone-buffer "*tele-gpt-error*" 'show))
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

(defun tele-gpt-begin-input (&optional init)
  (interactive)
  (when tele-gpt--busy-p
    (user-error "BUSY: Waiting for GPT complete its response..."))
  (tele-gpt-input-exit)
  (let ((dir (if (window-parameter nil 'window-side) 'bottom 'down))
        (buf (get-buffer-create tele-gpt--input-buffer-name)))
    (with-current-buffer buf
      (tele-gpt-input-mode)
      (erase-buffer)
      (when init (insert init))
      (call-interactively #'set-mark-command)
      (setf (point) (point-min)))
    (pop-to-buffer buf `((display-buffer-in-direction)
                         (reusable-frames . nil)
                         (direction . ,dir)
                         (dedicated . t)
                         (window-height . 30)))))

(defun tele-gpt-input-exit ()
  (interactive)
  (when-let ((buf (get-buffer tele-gpt--input-buffer-name)))
    (kill-buffer buf)))

(defun tele-gpt-input-send ()
  (interactive)
  (when tele-gpt--busy-p
    (user-error "BUSY: Waiting for GPT complete its response..."))
  (with-current-buffer tele-gpt--input-buffer-name
    (let ((input (buffer-substring-no-properties (point-min) (point-max))))
      (when (string-empty-p input)
        (user-error "No input to send"))
      (tele-gpt--send-input input)
      (tele-gpt-input-exit)
      (pop-to-buffer tele-gpt--session-name))))

(defun tele-gpt--send-input (input)
  (let ((message (tele-gpt--register-user-message input) ))
    (tele-gpt--display-message message)
    (setq tele-gpt--busy-p t)
    (spinner-start tele-gpt--spinner)
    (tele-gpt-send)))

(defvar tele-gpt-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'tele-gpt-input-send)
    (define-key map (kbd "C-c C-c") #'tele-gpt-input-send)
    (define-key map (kbd "C-c C-k") #'tele-gpt-input-exit)
    map))

(define-derived-mode tele-gpt-input-mode markdown-mode "TeleGPT Input"
  "Major mode for TeleGPT input mode.

\\<tele-gpt-input-mode-map>"
  (setq header-line-format '(" TeleGPT Input  |  C-c C-c to send, C-c C-k to cancel "))
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'tele-gpt-input-mode 'insert)))


;; Chat display

(defun tele-gpt--display-message (message)
  "Display the most recent history message."
  (unless (tele-gpt--valid-message-p message)
    (error "Message is not valid: %s" message))
  (with-current-buffer tele-gpt--session-name
    (tele-gpt-without-readonly
      (setf (point) (point-max))
      (let* ((message-content (plist-get message :content))
             (role (plist-get message :role)))
        (unless (bobp) (insert "\n\n"))
        (cond
         ((or (plist-get message :error) (eq role nil))
          (insert "## GPT Assistant [Error]\n\n"
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
          (insert (tele-gpt--format-response message))))

        ;; move point to bottom
        (setf (point) (point-max))))))

(defun tele-gpt--format-response (response)
  "Format GPT response for display."
  (let ((content (plist-get response :content))
        ;; (status (plist-get response :status))
        (tokens (plist-get response :tokens))
        ;; (time (plist-get response :time))
        (stop (plist-get response :stop)))
    (concat "## GPT Assistant "
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

(defun tele-gpt-clear-history ()
  (interactive)
  (when (y-or-n-p "Clear TeleGPT history forever?")
    (with-current-buffer tele-gpt--session-name
      (tele-gpt-without-readonly
        (setq tele-gpt--history '())
        (insert "\n\n\f\n# HISTORY CLEARED\n\f\n")))))

(defun tele-gpt--header-line ()
  "Display header line."
  (format " %s TeleGPT  |  C-RET to input a prompt"
          (if-let ((spinner (spinner-print tele-gpt--spinner)))
              (concat spinner " ")
            " ")))

(defvar tele-gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'tele-gpt-begin-input)
    (define-key map (kbd "C-c C-k") #'tele-gpt-clear-history)
    map))

(define-derived-mode tele-gpt-mode markdown-mode "TeleGPT"
  "Major mode for TeleGPT response mode.

\\<tele-gpt-mode-map>"
  (setq buffer-read-only t)
  (setq header-line-format '((:eval (tele-gpt--header-line))))
  (setq tele-gpt--spinner (spinner-create 'horizontal-breathing-long t))
  (add-hook 'kill-buffer-hook #'tele-gpt-kill-buffer-hook nil t))

;;;###autoload
(defun tele-gpt (&optional init)
  "Switch to or start a TeleGPT session.

If region is active, prefill input buffer with the region."
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (unless tele-gpt-openai-api-key
    (user-error "Must set `tele-gpt-openai-api-key'"))
  (let ((buf (get-buffer-create tele-gpt--session-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'tele-gpt-mode)
        (tele-gpt-mode))
      (let ((blank (string-empty-p (buffer-string))))
        (tele-gpt-without-readonly
          (when blank (insert "> Use the window below to input your prompt, then C-RET to send. "))
          (pop-to-buffer buf)
          (setf (point) (point-max))
          (when blank (tele-gpt-begin-input init)))))))

(provide 'tele-gpt)

;;; tele-gpt.el ends here

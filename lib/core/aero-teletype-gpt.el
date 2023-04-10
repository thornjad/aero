;;; aero-teletype-gpt.el --- Aero GPT client  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Jade Michael Thornton
;; Package-Requires: ((emacs "27.1") (ht "2.0") (markdown-mode "2.1") (spinner "1.7.4"))
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
;; Requires `aero/openai-api-key' to be set, doing so in an `init.local.el' is the right place to do this
;; in Aero.
;;
;; API Reference: https://platform.openai.com/docs/guides/chat
;;
;; TODO new session option, clear history and buffer

(declare-function markdown-mode "markdown-mode")
(declare-function pulse-momentary-highlight-region "pulse")

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))
(require 'url)
(require 'spinner)
(require 'json)
(require 'markdown-mode)

(require 'aero-lib)

;;; Code:

(defgroup aero/teletype-gpt nil
  "Aero Teletype GPT."
  :prefix "aero/gpt-"
  :group 'emacs-ml)

(defcustom aero/gpt-openai-api-key nil
  "An OpenAI API key."
  :group 'aero/teletype-gpt
  :type 'string)

(defface aero/gpt-system-ui
  '((t :inherit font-lock-builtin-face))
  "Face for Teletype GPT UI elements."
  :group 'aero/teletype-gpt)

(defface aero/gpt-tip
  '((t :inherit font-lock-comment-face))
  "Face for Teletype GPT tips."
  :group 'aero/teletype-gpt)

(defface aero/gpt-info
  '((t :inherit font-lock-comment-face :height 0.8))
  "Face for GPT info like tokens."
  :group 'aero/teletype-gpt)

(defface aero/gpt-error
  '((t :inherit error))
  "Face for GPT errors."
  :group 'aero/teletype-gpt)

;; (defface aero/gpt-status
;;   '((t :inherit font-lock-function-name-face :italic t))
;;   "Face for GPT current status."
;;   :group 'aero/teletype-gpt)

(defvar aero/gpt--debug-mode t)
(defvar aero/gpt--session-name "*Teletype GPT*")
(defvar aero/gpt--input-buffer-name "*Teletype GPT Input*")
(defvar-local aero/gpt--token-history nil)
(defvar-local aero/gpt--history '())
(defvar-local aero/gpt--response-buffer nil)
(defvar-local aero/gpt--busy-p nil)
(defvar-local aero/gpt--spinner nil)


;; API

(defun aero/teletype-gpt-send ()
  "Submit the current prompt to GPT."
  (interactive)
  (unless aero/gpt-openai-api-key
    (user-error "Must set `aero/gpt-openai-api-key'"))
  (let* ((prompt (aero/gpt--gather-prompt))
         (inhibit-message t)
         (message-log-max nil)
         (url-show-status aero/gpt--debug-mode)
         (url-show-headers aero/gpt--debug-mode)
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " aero/gpt-openai-api-key))))
         (url-request-data (encode-coding-string
                            ;; https://platform.openai.com/docs/api-reference/chat/create
                            (json-encode `(:model "gpt-3.5-turbo"
                                           :messages [,@prompt]
                                           :temperature nil
                                           :max_tokens nil))
                            'utf-8)))
    (url-retrieve "https://api.openai.com/v1/chat/completions"
                  (lambda (_)
                    (aero/gpt--register-response (aero/gpt--parse-response (current-buffer)))
                    (aero/gpt--display-last-message)
                    (setq-local aero/gpt--busy-p nil)
                    (spinner-stop aero/gpt--spinner)
                    (kill-buffer))
                  nil (not aero/gpt--debug-mode) nil)))

(defun aero/gpt--gather-prompt ()
  "Return a full prompt from chat history, prepended with a system prompt.

GPT-3 does not always respect the system prompt, though GPT-4 should be better at this."
  (let ((max-entries 10))
    (cons (list :role "system"
                :content (format "You are a large language model living in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely. Use Github-flavored Markdown formatting in all messages. Current date: %s" (format-time-string "%Y-%m-%d")))
          (nreverse (seq-take aero/gpt--history max-entries)))))

(defun aero/gpt--register-response (response)
  "Add GPT response to history."
  (push (list :role "assistant" :content response)
        aero/gpt--history))

(defun aero/gpt--register-user-message (input)
  "Add user message to history."
  (push (list :role "user" :content (string-trim input " \t\n\r"))
        aero/gpt--history))

(defun aero/gpt--parse-response (buffer)
  "Parse the GPT response in URL BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when aero/gpt--debug-mode (clone-buffer "*aero-gpt-error*" 'show))
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
            (let ((choices (plist-get (plist-get response :choices) 0)))
              (list :content (string-trim (plist-get (plist-get choices :message) :content))
                    :tokens (plist-get response :usage)
                    :time (plist-get response :created)
                    :stop (plist-get choices :finish_reason)
                    :status status)))
           ((plist-get response :error)
            (let* ((error-plist (plist-get response :error))
                   (error-msg (plist-get error-plist :message))
                   (error-type (plist-get error-plist :type)))
              (list :content nil :status (concat status ": " error-type))))
           ((eq response 'json-read-error)
            (list :content nil :status (concat status ": Malformed JSON in response.")))
           (t (list :content nil :status (concat status ": Could not parse HTTP response."))))
        (list :content nil :status (concat status ": Could not parse HTTP response."))))))


;; User input

(defun aero/teletype-gpt-begin-input ()
  (interactive)
  (when aero/gpt--busy-p
    (user-error "BUSY: Waiting for GPT complete its response..."))
  (aero/gpt-input--exit)
  (let ((dir (if (window-parameter nil 'window-side) 'bottom 'down))
        (buf (get-buffer-create aero/gpt--session-name)))
    (with-current-buffer buf
      (aero/teletype-gpt-input-mode)
      (erase-buffer)
      (call-interactively #'set-mark-command)
      (setf (point) (point-min)))
    (pop-to-buffer buf `((display-buffer-in-direction)
                         (direction . ,dir)
                         (dedicated . t)
                         (window-height . fit-window-to-buffer)))))

(defun aero/gpt-input--exit ()
  (kill-buffer aero/gpt--input-buffer-name))

(defun aero/teletype-gpt-input-send ()
  (interactive)
  (when (not (eq major-mode #'aero/teletype-gpt-input-mode))
    (user-error "Must be called from the Aero Teletype GPT input window. Try `aero/teletype-gpt-begin-input' first."))
  (when aero/gpt--busy-p
    (user-error "BUSY: Waiting for GPT complete its response..."))
  (let ((input (buffer-substring-no-properties (point-min) (point-max))))
    (when (string-empty-p input)
      (user-error "No input to send"))
    (aero/gpt--send-input input))
  (aero/gpt-input--exit))

(defun aero/gpt--send-input (input)
  (aero/gpt--register-user-message input)
  (aero/gpt--display-last-message)
  (setq-local aero/gpt--busy-p t)
  (spinner-start aero/gpt--spinner)
  (aero/teletype-gpt-send))

(defun aero/gpt-input--post-command ()
  "Resize window after input."
  (let ((max-lines (line-number-at-pos (point-max))))
    (fit-window-to-buffer)
    (enlarge-window (- max-lines (window-text-height)))))

(defvar aero/teletype-gpt-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'aero/teletype-gpt-input-send)
    map))

(define-derived-mode aero/teletype-gpt-input-mode markdown-mode "TeletypeGPT Input"
  "Major mode for Aero Teletype GPT input mode.

\\<aero/teletype-gpt-input-mode-map>"
  (add-hook 'post-command-hook #'aero/gpt-input--post-command nil t))


;; Chat display

(defun aero/gpt--insert-tip ()
  (aero/without-readonly
    (insert (propertize "Press C-return to begin a prompt.\n" 'face 'aero/gpt-tip))))

(defun aero/gpt--display-last-message ()
  "Display the most recent history message."
  (aero/with-buffer-max-excursion aero/gpt--session-name
    (aero/without-readonly
      (let* ((message (car (last aero/gpt--history)))
             (role (plist-get message :role)))
        (unless (bobp) (insert "\n\n"))
        (cond
         ((string= role "user")
          (insert "# User\n\n" message))

         ((string= role "assistant")
          (insert (aero/gpt--format-response message)))

         ((eq role nil)
          (insert "# GPT Assistant [Error]\n\n"
                  (propertize (plist-get message :status) 'face 'aero/gpt-error))))))))

(defun aero/gpt--format-response (response)
  "Format GPT response for display."
  (let ((content (plist-get response :content))
        ;; (status (plist-get response :status))
        (tokens (plist-get response :tokens))
        ;; (time (plist-get response :time))
        (stop (plist-get response :stop)))
    (insert "# GPT Assistant "
            ;; Tokens
            (propertize (format "Tokens: %s (%s prompt, %s response)"
                                (plist-get tokens :total_tokens)
                                (plist-get tokens :prompt_tokens)
                                (plist-get tokens :completion_tokens))
                        'face 'aero/gpt-info)
            "\n\n" content "\n\n"
            (cond
             ((string= stop "length") "Stop Reason: Token Limit")
             ((string= stop "content_filter") "Stop Reason: Content Filter Flag")
             (t ""))
            "\f\n")))

(defun aero/gpt-kill-buffer-hook ()
  "Kill response buffer hook."
  (spinner-stop aero/gpt--spinner)
  (setq-local aero/gpt--history '())
  (setq-local aero/gpt--response-buffer nil))

(defun aero/gpt--header-line ()
  "Display header line."
  (format " %s — %s history — %s tokens (% prompt, %s response)"
          (if-let ((spinner (spinner-print aero/gpt--spinner)))
              (concat spinner " ")
            " ")
          (length aero/gpt--history)
          (plist-get aero/gpt--token-history :total)
          (plist-get aero/gpt--token-history :prompt)
          (plist-get aero/gpt--token-history :response)))

(defvar aero/teletype-gpt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<return>") #'aero/teletype-gpt-begin-input)
    map))

(define-derived-mode aero/teletype-gpt-mode markdown-mode "TeletypeGPT"
  "Major mode for Aero Teletype GPT response mode.

\\<aero/teletype-gpt-mode-map>"
  (setq-local buffer-read-only t)
  (setq-local header-line-format `((:eval (aero/gpt--header-line))))
  (setq-local aero/gpt--spinner (spinner-create 'horizontal-breathing-long t))
  (add-hook 'kill-buffer-hook #'aero/gpt-kill-buffer-hook nil t))

;;;###autoload
(defun aero-teletype-gpt ()
  "Switch to or start a Teletype GPT session."
  (interactive)
  (unless aero/gpt-openai-api-key
    (user-error "Must set `aero/gpt-openai-api-key'"))
  (let ((buf (get-buffer-create aero/gpt--session-name)))
    (with-current-buffer buf
      (unless aero/teletype-gpt-mode (aero/teletype-gpt-mode))
      (when (string-empty-p (buffer-string)) (aero/gpt--insert-tip))
      (pop-to-buffer buf)
      (setf (point) (point-max)))))

(provide 'aero-teletype-gpt)

;;; aero-teletype-gpt.el ends here

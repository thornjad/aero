;;; aero-teletype-gpt.el --- Aero GPT client  -*- lexical-binding: t; -*-
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
;; A simple markdown-based GPT client for Aero Emacs.
;;
;; Requires `aero/openai-api-key' to be set, doing so in an `init.local.el' is the right place to do this
;; in Aero.
;;
;; TODO usage tracker, insert at end of response?

(declare-function markdown-mode "markdown-mode")
(declare-function pulse-momentary-highlight-region "pulse")

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))
(require 'url)
(require 'json)
(require 'map)
(require 'text-property-search)

;;; Code:

(defgroup aero/gpt nil
  "A GPT Client for Aero."
  :group 'emacs-ml)

(defcustom aero/gpt-openai-api-key nil
  "An OpenAI API key."
  :group 'aero/gpt
  :type 'string)

(defvar aero/gpt-max-tokens nil
  "Max tokens per response.

This is roughly correlated with the number of words in the response. Figure 100-400 for shorter
answers, more for longer responses.

When nil, GPT will generally respond at around 40% of the total token count of the conversation so
far, so messages naturally grow as the dialogue develops.")

(defvar aero/gpt-model "gpt-3.5-turbo"
  "The GPT model to be used.

The list of models supported is documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility.

Be aware that different models have different pricing structures, refer to the OpenAI pricing page
for specifics.")

(defvar aero/gpt-temp nil
  "The temperature of the GPT response.

This is a number between 0.0 and 2.0 which informs GPT's randomness. A nil value will not send this configuration to GPT.

See https://platform.openai.com/docs/api-reference/completions/create#completions/create-temperature
for details.")

(defvar aero/gpt-main-session "*Teletype GPT*"
  "Name of the main Aero GPT session.")

(defvar-local aero/gpt--system-directive
    "You are a large language model living in Emacs; you are a helpful assistant and a careful, wise programmer. Respond concisely. Use Markdown formatting."
  "The system directive helps set GPT's response behavior.

See https://platform.openai.com/docs/guides/chat/introduction.")

(defvar aero/gpt--debug-mode nil
  "If non-nil, log more.")

(defvar aero/gpt--busy nil)

(defun aero/teletype-gpt-send ()
  "Submit the current prompt to GPT."
  (interactive)
  (if aero/gpt--busy
      (user-error "GPT already busy, please wait until the previous request finishes.")
    (message "Querying GPT...")
    (let* ((prompt (aero/gpt--create-prompt))
           (buf (current-buffer))
           (marker (point-marker))
           (inhibit-message t)
           (message-log-max nil)
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " aero/gpt-openai-api-key))))
           (url-request-data (encode-coding-string
                              (json-encode `(:model ,aero/gpt-model
                                             :messages [,@prompt]
                                             :temperature ,aero/gpt-temp
                                             :max_tokens ,aero/gpt-max-tokens))
                              'utf-8)))
      (setq aero/gpt--busy t)
      (url-retrieve "https://api.openai.com/v1/chat/completions"
                    (lambda (_)
                      (aero/gpt--insert-response
                       (aero/gpt--parse-response (current-buffer))
                       buf marker)
                      (setq aero/gpt--busy nil)
                      (kill-buffer))
                    nil (not aero/gpt--debug-mode) nil))))

(defun aero/gpt--create-prompt ()
  "Return a full prompt from the contents of this buffer."
  (save-excursion
    (setf (point) (point-max))
    (let ((max-entries nil)
          (prop) (prompts (list)))
      (while (and (or (not max-entries) (>= max-entries 0))
                  (setq prop (text-property-search-backward
                              'gptel 'response
                              (not (not (get-char-property
                                         (max (point-min) (1- (point)))
                                         'gptel))))))
        (push (list :role (if (prop-match-value prop) "assistant" "user")
                    :content (string-trim
                              (buffer-substring-no-properties (prop-match-beginning prop)
                                                              (prop-match-end prop))
                              "[*# \t\n\r]+"))
              prompts)
        (and max-entries (cl-decf max-entries)))
      (cons (list :role "system" :content aero/gpt--system-directive) prompts))))

(defun aero/gpt--insert-response (response buffer marker)
  "Insert GPT's RESPONSE into BUFFER at MARKER."
  (let ((content (plist-get response :content))
        (status (plist-get response :status)))
    (message "Querying GPT... Done.")
    (if content
        (with-current-buffer buffer
          (put-text-property 0 (length content) 'aero/gpt 'response content)
          (setf (point) marker)
          (unless (bobp) (insert-before-markers-and-inherit "\n\n"))
          (let ((p (point)))
            (insert content)
            (pulse-momentary-highlight-region p (point)))
          (when aero/teletype-gpt-mode
            (insert "\n\n### ")
            (message "GPT Ready")))
      (error "GPT response error: %s" status))))

(defun aero/gpt--parse-response (buffer)
  "Parse the GPT response in BUFFER."
  (when (buffer-live-p buffer)
    (when aero/gpt--debug-mode
      (with-current-buffer buffer (clone-buffer "*aero-gpt-error*" 'show)))
    (with-current-buffer buffer
      (if-let* ((status (buffer-substring (line-beginning-position) (line-end-position)))
                (json-object-type 'plist)
                (response (progn
                            (forward-paragraph)
                            (let ((json-str (decode-coding-string
                                             (buffer-substring-no-properties (point) (point-max))
                                             'utf-8)))
                              (condition-case nil
                                  (json-read-from-string json-str)
                                (json-readtable-error 'json-read-error))))))
          (cond
           ((string-match-p "200 OK" status)
            (list :content (string-trim (map-nested-elt response
                                                        '(:choices 0 :message :content)))
                  :status status))
           ((plist-get response :error)
            (let* ((error-plist (plist-get response :error))
                   (error-msg (plist-get error-plist :message))
                   (error-type (plist-get error-plist :type)))
              (message "ChatGPT error: %s" error-msg)
              (list :content nil :status (concat status ": " error-type))))
           ((eq response 'json-read-error)
            (message "ChatGPT error: Malformed JSON in response.")
            (list :content nil :status (concat status ": Malformed JSON in response.")))
           (t (message "ChatGPT error: Could not parse HTTP response.")
              (list :content nil :status (concat status ": Could not parse HTTP response."))))
        (message "ChatGPT error: Could not parse HTTP response.")
        (list :content nil :status (concat status ": Could not parse HTTP response."))))))

(define-minor-mode aero/teletype-gpt-mode
  "Minor mode for Aero Teletype GPT."
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-<return>") #'aero/teletype-gpt-send)
            map))

;;;###autoload
(defun aero-teletype-gpt (&optional initial new)
  "Switch to or start a Teletype GPT session.

If NEW, start a new session. If region is active, it is used as the INITIAL prompt."
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))

  (unless aero/gpt-openai-api-key
    (user-error "Must set `aero/gpt-openai-api-key'"))

  (let ((buf (get-buffer-create (if new
                                    (generate-new-buffer-name aero/gpt-main-session)
                                  aero/gpt-main-session))))
    (with-current-buffer buf
      (require 'markdown-mode)
      (markdown-mode)
      (unless aero/teletype-gpt-mode (aero/teletype-gpt-mode +1))
      (when (bobp) (insert (or initial "### ")))
      (pop-to-buffer buf)
      (setf (point) (point-max))
      (skip-chars-backward "\t\r\n")
      (message "Send your prompt with C-Return"))))

(defun aero-teletype-gpt-new (&optional initial)
  "Start a new Teletype GPT session.

If region is active, it is used as the INITIAL prompt."
  (interactive)
  (funcall-interactively #'aero-teletype-gpt initial t))

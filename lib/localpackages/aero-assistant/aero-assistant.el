;;; aero-assistant.el --- Aero AI Assistant client  -*- lexical-binding: t; -*-
;;
;; Author: Jade Michael Thornton
;; Copyright (c) 2023 Jade Michael Thornton
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

;; Using Aero Assistant for Git Commit Messages in Magit:
;;
;; The `aero/assistant-commit-message' function can add an Aero Assistant- generated commit message.
;; This function requires [Magit](https://github.com/magit/magit).
;;
;; Whenever you commit using Magit, calling `aero/assistant-commit-message' will automatically
;; generate a commit message based on the staged git changes

;;; License:
;;
;; Copyright (c) 2023 Jade Michael Thornton
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
(declare-function pulse-momentary-highlight-region "pulse")

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

(defvar aa--model "GPT 4")
(defvar aa-commit-model "GPT 3.5")
(defvar aa--model-options
  '("GPT 3.5"
    "GPT 4"
    "DALL-E"
    ))
(defvar aa--openai-models '("GPT 4" "GPT 3.5" "DALL-E"))
(defvar aa--model-name-map
  #s(hash-table size 10 test equal data
                ("GPT 3.5" "gpt-3.5-turbo-1106"
                 "GPT 4" "gpt-4-1106-preview"
                 "DALL-E" "dall-e-3"
                 )))
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
    (cond
     ((member aa--model aa--openai-models)
      (require 'aero-assistant-openai)
      (aa--send-openai model))
     ((string= aa--model "StableLM")
      (require 'aero-assistant-stability)
      (aa--send-stability model)))))

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
    (evil-set-initial-state 'aa-input-mode 'insert)))


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
            (when show-image (concat "![](" content ")\n\n"))
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

(defun aa--set-model ()
  "Prompt user to set the Assistant model and verify key if required."
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
  (add-hook 'kill-buffer-hook #'aa-kill-buffer-hook nil t))

;;;###autoload
(defun aero/assistant (&optional init)
  "Switch to or start an Aero Assistant session.

If region is active, prefill input buffer with the region."
  (interactive (list (and (use-region-p) (buffer-substring (region-beginning) (region-end)))))
  (unless (get-buffer aa--session-name)
    (aa--set-model))
  (let ((buf (get-buffer-create aa--session-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'aa-mode)
        (aa-mode))
      (let ((blank (string-empty-p (buffer-string))))
        (aa-without-readonly
          (pop-to-buffer buf)
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

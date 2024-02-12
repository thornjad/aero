;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2023-2024 Jade Michael Thornton
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
;;; Code:

;; Aero LLM assistant interface
(package! aero-assistant :local :load-path "lib/localpackages/aero-assistant"
  :after (general)
  :commands (aero/assistant aero/assistant-commit-message)
  :custom (aero/assistant-openai-api-key openai-api-key)
  :init (aero-leader-def "aic" 'aero/assistant))

;; Required by chatgpt-shell
(package! shell-maker
  (:repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

;; GPT and DALL-E interface
(package! chatgpt-shell "xenodium/chatgpt-shell"
  :requires shell-maker
  :after general
  :commands (chatgpt-shell
             dall-e-shell chatgpt-shell-send-and-review-region
             chatgpt-shell-write-git-commit chatgpt-shell-explain-code
             chatgpt-shell-proofread-region chatgpt-shell-refactor-code
             chatgpt-shell-restore-session-from-transcript
             chatgpt-shell-generate-unit-test)

  :custom
  (chatgpt-shell-openai-key openai-api-key)
  (dall-e-shell-openai-key openai-api-key)
  (chatgpt-shell-model-versions '("gpt-4-1106-preview" "gpt-3.5-turbo-16k-0613"
                                  "gpt-3.5-turbo"))
  (chatgpt-shell-welcome-function nil) ; disable welcome message
  (chatgpt-shell-system-prompt 0)
  (chatgpt-shell-system-prompts '(("Aero" . "You will act as a brilliant and experienced senior software engineer working in Emacs; you are a helpful assistant and a careful, wise programmer.
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
When using Typescript, assume the user is using version 4.8 or newer.")))

  ;; :init
  ;; (aero-leader-def
  ;;   "aic" '(chatgpt-shell :wk "chat shell")
  ;;   "air" '(chatgpt-shell-send-and-review-region :wk "send region with review")
  ;;   "aig" '(chatgpt-shell-write-git-commit :wk "write git commit")
  ;;   "aie" '(chatgpt-shell-explain-code :wk "explain code in region")
  ;;   "aip" '(chatgpt-shell-proofread-region :wk "proofread in region")
  ;;   "aif" '(chatgpt-shell-refactor-code :wk "refactor code in region")
  ;;   "ais" '(chatgpt-shell-restore-session-from-transcript :wk "restore session from transcript")
  ;;   "aiu" '(chatgpt-shell-generate-unit-test :wk "generate unit test"))

  :config
  ;; Seems to reset itself unless put in config
  (setq chatgpt-shell-history-path aero-cache-dir))

;; Works best with company-box, so we consider it a requirement
(package! copilot (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :after (company-box general)
  :hook ((prog-mode eshell-mode) . copilot-mode)
  :custom (copilot-idle-delay 0.5)
  :config
  (general-define-key
   :states '(insert visual motion)
   :keymaps 'copilot-mode-map
   (kbd "C-<tab>") 'copilot-accept-completion
   (kbd "C-c C-n") 'copilot-next-completion
   (kbd "C-c C-p") 'copilot-previous-completion))

(provide 'aero-ai)
;;; aero-ai.el ends here

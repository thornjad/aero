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
(package! assist :local :load-path "lib/localpackages/assist"
  :after (general)
  :commands (assist-chat assist-commit-message assist-diff-qa-steps)
  :custom
  (assist-openai-api-key openai-api-key)
  (assist-anthropic-api-key anthropic-api-key)
  :init
  (aero-leader-def
    "aic" 'assist-chat))

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

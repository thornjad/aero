;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
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
;;; Code:

(use-package python-mode :straight t
  :mode "\\.py\\'"
  :init
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "--simple-prompt -i --pprint")
    (setq python-shell-interpreter "python3"))
  (setq-default python-indent-offset 4)

  :config
  (use-package pyvenv :straight t)

  (use-package lsp-python :straight t :disabled
    :config
    (require 'lsp-python)
    (add-hook 'python-mode-hook #'lsp-python-enable))

  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (setq-local indent-tabs-mode nil)

    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil))

  (add-hook 'python-mode-hook #'my-python-mode-hook)

  (defun python-shell-send-line (&optional vis)
    "send the current line to the inferior python process"
    (interactive "P")
    (save-excursion
      (end-of-line)
      (let ((end (point)))
        (beginning-of-line)
        (python-shell-send-region (point) end vis "eval line"))))

  (defun current-line-empty-p ()
    (save-excursion
      (beginning-of-line)
      (looking-at "[[:space:]]*$")))

  (defun python-shell-send-line-and-go ()
    (interactive)
    (python-shell-send-line)
    (python-shell-switch-to-shell))

  (aero-mode-leader-def
    :keymaps 'python-mode-map
    "p" 'run-python
    "s" '(:ignore t :wk "shell send")
    "sd" 'elpy-shell-send-defun
    "sD" 'elpy-shell-send-defun-and-go
    "sl" 'python-shell-send-line
    "sL" 'python-shell-send-line-and-go
    "sb" 'elpy-shell-send-buffer
    "sB" 'elpy-shell-send-buffer-and-go
    "sr" 'elpy-shell-send-region-or-buffer
    "sR" 'elpy-shell-send-region-or-buffer-and-go
    "sc" 'elpy-shell-send-defclass
    "sC" 'elpy-shell-send-defclass-and-go
    "e" '(:ignore t :wk "change executable")
    "ey" 'elpy-switch-to-pypy
    "ec" 'elpy-switch-to-cpython
    "ei" 'elpy-switch-to-ipithon
    "ej" 'elpy-switch-to-jupyter
    "k" '(:ignore t :wk "kill")
    "kk" 'elpy-shell-kill
    "kA" 'elpy-shell-kill-all
    "g" '(:ignore t :wk "go")
    "ge" 'elpy-shell-goto-last-error
    "gi" 'elpy-shell-goto-import-header
    "d" '(:ignore t :wk "pdb"))
  (general-define-key
   :keymaps 'python-mode-map
   "s-e" 'python-shell-send-defun
   "C-<return>" 'python-shell-send-line)

  (defvar pdb-completion-at-point-script
    (concat
     "import rlcompleter;"
     "__ECAP_N=locals().copy();__ECAP_N.update(**globals());"
     "__ECAP_T='''%s''';"
     "print(';'.join("
     "getattr(rlcompleter.Completer(__ECAP_N), 'attr_matches' if '.' in __ECAP_T else 'global_matches')"
     "(__ECAP_T)))"))

  (defun pdb-completion-at-point-function ()
    "Complete at point if in pdb prompt."
    (let* ((buffer (current-buffer))
           (process (get-buffer-process buffer)))
      ;; Get completion only if there are process and we are at pdb prompt
      (when (and
             process
             (save-excursion
               (forward-line 0)
               (looking-at python-shell-prompt-pdb-regexp)))
        (let* ((end (point))
               ;; Beginning of prefix to search completion for
               ;; Get whole input and search backwards for delimiters
               (start (save-excursion (comint-goto-process-mark) (point)))
               (start (or (save-excursion
                            (when (re-search-backward
                                   "[([{]\\|[])}]\\|[[:space:]]\\|\\(?:\\(?:\\*\\*\\|//\\|<<\\|>>\\|[%&*+/|^-]\\)?=\\)"
                                   start t)
                              (1+ (point))))
                          start)))
          (when (> end start)
            (let ((prefix (buffer-substring-no-properties start end))
                  ;; this regexp is used to determine that output redirect is done
                  (comint-prompt-regexp (concat "^" python-shell-prompt-pdb-regexp)))
              (with-temp-buffer
                (comint-redirect-send-command-to-process
                 (format pdb-completion-at-point-script prefix)
                 (current-buffer) process nil t)
                ;; wait for command output
                (with-current-buffer buffer
                  (while (null comint-redirect-completed)
                    (accept-process-output nil 0.1)))
                (goto-char (point-min))
                ;; Skip first line in case process echoes input
                (unless (= (count-lines (point-min) (point-max)) 1)
                  (forward-line 1))
                (let ((completions (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                  ;; return nil instead of empty completions so other
                  ;; completion functions can provide their completions
                  (unless (string-empty-p completions)
                    (list start end (split-string completions ";")))))))))))

  (add-hook
   'inferior-python-mode-hook
   (lambda ()
     (add-hook 'completion-at-point-functions
               'pdb-completion-at-point-function nil t))))

(use-package ein :straight t
  :mode "\\.ipynb\\'"
  :config
  (aero-leader-def
    "aj" '(:ignore t :wk "jupyter")
    "ajn" '(ein:run :wk "notebook")
    "ajl" '(ein:login :wk "log in to server")))

(use-package hy-mode :straight t
  :mode "\\.hy\\'"
  :config
  (aero-mode-leader-def
    :keymaps 'hy-mode-map
    "h" 'run-hy
    "p" 'run-python))

(use-package elpy :straight t
  :hook ((python-mode
          ein-mode)
         . elpy-mode)
  :config
  (elpy-enable)

  (defun elpy-switch-to-cpython ()
    "Switch to using CPython shell."
    (interactive)
    (setq python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"
          python-shell-prompt-detect-failure-warning t))
  (defun elpy-switch-to-pypy ()
    "Switch to using CPython shell."
    (interactive)
    (when (executable-find "pypy3")
      (setq python-shell-interpreter "pypy3"
            python-shell-interpreter-args "-i"
            python-shell-prompt-detect-failure-warning t)))
  (defun elpy-switch-to-ipython ()
    "Switch to using IPython shell."
    (interactive)
    (when (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt"
            python-shell-prompt-detect-failure-warning t)))
  (defun elpy-switch-to-jupyter ()
    "Switch to using Jupyter shell."
    (interactive)
    (when (executable-find "jupyter")
      (setq python-shell-interpreter "jupyter"
            python-shell-interpreter-args "console --simple-prompt"
            python-shell-prompt-detect-failure-warning nil)))

  (defun elpy-goto-import-header ()
    "Jump to the import header."
    (interactive)
    (evil-set-jump)
    (goto-char (point-min))
    (re-search-forward "^import"))

  (defun elpy-shell-goto-last-error ()
    "Jump to the last error reported by the shell."
    (interactive)
    (let ((filename (buffer-file-name)))
      (elpy-shell-switch-to-shell)
      (goto-char (point-max))
      (search-backward filename nil t)
      (while (condition-case nil
                 (progn (compile-goto-error) nil)
               (error t))
        (forward-line -1))))

  (aero-mode-leader-def
    :keymaps 'elpy-mode-map
    "p" 'run-python
    "s" '(:ignore t :wk "shell send")
    "sd" 'elpy-shell-send-defun
    "sD" 'elpy-shell-send-defun-and-go
    "sb" 'elpy-shell-send-buffer
    "sB" 'elpy-shell-send-buffer-and-go
    "sr" 'elpy-shell-send-region-or-buffer
    "sR" 'elpy-shell-send-region-or-buffer-and-go
    "sc" 'elpy-shell-send-defclass
    "sC" 'elpy-shell-send-defclass-and-go
    "e" '(:ignore t :wk "change executable")
    "ey" 'elpy-switch-to-pypy
    "ec" 'elpy-switch-to-cpython
    "ei" 'elpy-switch-to-ipithon
    "ej" 'elpy-switch-to-jupyter
    "k" '(:ignore t :wk "kill")
    "kk" 'elpy-shell-kill
    "kA" 'elpy-shell-kill-all
    "g" '(:ignore t :wk "go")
    "ge" 'elpy-shell-goto-last-error
    "gi" 'elpy-shell-goto-import-header))

(provide 'aero-python)

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2024 Jade Michael Thornton
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

(require 'aero-prelude)

(package! python-mode :auto
  :after (general)
  :mode "\\.py\\'"
  :custom
  (python-indent-guess-indent-offset t)
  (python-indent-guess-indent-offset-verbose nil)
  :init
  (setq-default python-shell-interpreter "python3")
  (setq-default python-indent-offset 4)

  :config
  (defvar python-mode-initialized nil)

  (aero-mode-leader-def
    :keymaps 'python-mode-map
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
    "gi" 'elpy-shell-goto-import-header
    "d" '(:ignore t :wk "pdb")
    "di" '(aero/insert-pdb :wk "insert pdb"))
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
      (defvar python-shell-prompt-pdb-regexp)
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
                (setf (point) (point-min))
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

(defun pdb-poetry ()
  "Like `py-pdb' but with poetry."
  (interactive)
  (require 'gud)
  (let ((command-line
         (read-from-minibuffer
          "Run pdb like this: "
          (concat
           "poetry run python3 -m pdb "
           (if (and (featurep 'tramp) (tramp-tramp-file-p (buffer-file-name)))
               (tramp-file-name-localname (tramp-dssect-file-name (buffer-file-name)))
             (buffer-file-name))))))
    (if command-line (pdb command-line) (error "command required"))))

(package! elpy (:host github :repo "jorgenschaefer/elpy")
  :hook ((python-mode ein-mode) . elpy-mode)
  :config
  (setq elpy-rpc-virtualenv-path 'default
        elpy-rpc-python-command "python3"
        py-return-key #'py-newline-and-indent)
  (elpy-enable)

  (defun elpy-goto-import-header ()
    "Jump to the import header."
    (interactive)
    (evil-set-jump)
    (setf (point) (point-min))
    (re-search-forward "^import"))

  (defun elpy-shell-goto-last-error ()
    "Jump to the last error reported by the shell."
    (interactive)
    (let ((filename (buffer-file-name)))
      (elpy-shell-switch-to-shell)
      (setf (point) (point-max))
      (search-backward filename nil t)
      (while (condition-case nil
                 (progn (compile-goto-error) nil)
               (error t))
        (forward-line -1))))

  (aero-mode-leader-def
    :keymaps 'elpy-mode-map
    "p" 'run-python
    "'" 'elpy-shell-switch-to-shell
    "s" '(:ignore t :wk "shell send")
    "ss" '(elpy-shell-send-region-or-buffer :wk "send region or buffer")
    "sS" '(elpy-shell-send-region-or-buffer-and-go :wk "send region or buffer and go")
    "sd" '(elpy-shell-send-defun :wk "send defun")
    "sD" '(elpy-shell-send-defun-and-go :wk "send defun and go")
    "sb" '(elpy-shell-send-buffer :wk "send buffer")
    "sB" '(elpy-shell-send-buffer-and-go :wk "send buffer and go")
    "sc" '(elpy-shell-send-defclass :wk "send defclass")
    "sC" '(elpy-shell-send-defclass-and-go :wk "send defclass and go")
    "k" '(:ignore t :wk "kill")
    "kk" '(elpy-shell-kill :wk "kill shell")
    "kA" '(elpy-shell-kill-all :wk "kill all shells")
    "g" '(:ignore t :wk "go")
    "ge" '(elpy-shell-goto-last-error :wk "goto last error")
    "gi" '(elpy-shell-goto-import-header :wk "goto import header")
    "t" '(elpy-test :wk "run test")
    "d" '(:ignore t :wk "debug")
    "dd" '(elpy-pdb-debug-buffer :wk "debug buffer")
    "db" '(elpy-pdb-toggle-breakpoint-at-point :wk "toggle breakpoint")
    "r" '(:ignore t :wk "refactor")
    "rb" '(elpy-black-fix-code :wk "black format")
    "rr" '(elpy-refactor-rename :wk "rename")
    "rv" '(elpy-refactor-extract-variable :wk "extract variable")
    "rf" '(elpy-refactor-extract-function :wk "extract function")
    "ri" '(elpy-refactor-inline :wk "inline variable")
    "rF" '(elpy-format-code :wk "format buffer or region")))

(provide 'aero-python)

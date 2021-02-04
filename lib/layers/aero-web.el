;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
;;
;; Copyright (c) 2018-2020 Jade Michael Thornton
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

(require 'aero-lib)

(use-package web-mode :straight t
  :mode
  "\\.\\(tpl\\|php\\|xml\\|html?\\|djhtml\\|erb\\|eco\\|ejs\\|svg\\)\\'"
  :config
  (setq web-mode-engines-alist
      '(("ctemplate" . "\\.tpl\\'"))))

(use-package emmet-mode :straight t
  :load-path "lib/packages/emmet-mode/"
  :hook ((web-mode html-mode css-mode scss-mode rjsx-mode js-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'rjsx-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t)))
  (add-hook
   'js-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :straight t
  :mode "\\.s?css\\'")


;; js and jsx

(use-package js2-mode :straight t :defer t)

;; (use-package javascript-mode
;;   :mode "\\.jsx?\\'")

(use-package rjsx-mode
  :load-path "lib/packages/rjsx-mode/"
  :mode "\\.jsx?\\'"

  :config
  ;; because we want C-d to scroll up normally
  (evil-define-key 'insert rjsx-mode-map
    (kbd "C-d") 'rjsx-delete-creates-full-tag)
  (evil-define-key 'normal rjsx-mode-map
    (kbd "C-d") 'evil-scroll-down))

(eval-when-compile (defvar emmet-expand-jsx-className?))
(add-hook 'js-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))

(use-package json-mode :straight t
	:mode "\\.json\\'")

(use-package yaml-mode :straight t
  :mode "\\.ya?ml\\'")

;; the rest

(use-package coffee-mode :straight t
  :mode "\\.coffee\\'")

(use-package restclient
	:straight t
  :defer t
  :commands (restclient-mode)
  :mode ("\\.http\\'" . restclient-mode)

  :init
  (aero-leader-def
    "wR" 'restclient-mode
    "wr" 'aero/restclient-scratch)

  :config
  (use-package company-restclient :straight t
    :config
    (add-to-list 'company-backends 'company-restclient))

  (aero-mode-leader-def
    :keymaps 'restclient-mode-map
    "RET" '(restclient-http-send-current-stay-in-window :wk "Run query at point")
    "c" '(restclient-http-send-current :wk "Run query at point and focus")
    "r" '(restclient-http-send-current-raw :wk "Run query, no pretty print")
    "n" 'restclient-jump-next
    "p" 'restclient-jump-prev
    "." 'restclient-mark-current
    "y" 'restclient-copy-curl-command))


;;; aero/npm commands
;; NPM commands based on npm-mode (https://github.com/mojochao/npm-mode)

(require 'json)

(defun aero/npm--project-file ()
  "Return the path to project file or nil."
  (let ((dir (locate-dominating-file default-directory "package.json")))
    (unless dir
      (aero/log-error "Aero/npm error: cannot find package.json"))
    (expand-file-name "package.json" dir)))

(defun aero/npm--get-project-property (prop)
  "Get the given PROP from the current project file."
  (let* ((project-file (aero/npm--project-file))
         (json-object-type 'hash-table)
         (json-contents
          (with-temp-buffer
            (insert-file-contents project-file)
            (buffer-string)))
         (json-hash (json-read-from-string json-contents))
         (value (gethash prop json-hash))
         (commands (list)))
    (cond ((hash-table-p value)
           (maphash
            (lambda (key value)
              (setq commands
                    (append
                     commands
                     (list (list key (format "%s %s" "npm" key))))))
            value)
           commands)
          (t value))))

(defun aero/npm--exec (cmd &optional comint)
  "Execute CMD in COMINT or new process."
  (let ((compilation-buffer-name-function
         (lambda (mode)
           (format "*npm:%s - %s*"
                   (aero/npm--get-project-property "name") cmd))))
    (aero/log-info (concat "Running npm " cmd))
    (compile (format "npm %s" cmd) comint)))

(defun aero/npm-init (&optional comint)
  "Run npm init."
  (interactive)
  (aero/log-info "Initializing npm project...")
  (let ((compilation-buffer-name-function
         (lambda (mode) "" "npm init")))
    (compile "npm init" comint)))

(defun aero/npm-install ()
  "Run npm install."
  (interactive)
  (aero/npm--exec "install"))

(defun aero/npm-install-save (deps)
  "Run npm install and save DEPS."
  (interactive "sPackage to save as deps: ")
  (aero/npm--exec (format "install %s --save" deps)))

(defun aero/npm-install-save-dev (deps)
  "Run npm install and save DEPS."
  (interactive "sPackage to save as dev deps: ")
  (aero/npm--exec (format "install %s --save-dev" deps)))

(defun aero/npm-uninstall (deps)
  "Run npm uninstall on DEPS."
  (interactive
   (list (completing-read "Uninstall deps: " (aero/npm--get-project-property "dependencies"))))
  (aero/npm--exec (format "uninstall %s" deps)))

(defun aero/npm-list ()
  "List npm dependencies."
  (interactive)
  (aero/npm--exec "list --depth=0"))

(defun aero/npm-run (script &optional comint)
  "Run npm SCRIPT in COMINT."
  (interactive
   (list (completing-read "Run script: " (aero/npm--get-project-property "scripts"))
         (consp current-prefix-arg)))
  (aero/npm--exec (format "run %s" script) comint))

(defun aero/npm-run-test (&optional comint)
  "Run npm test in COMINT."
  (interactive (list (consp current-prefix-arg)))
  (aero/npm--exec "test" comint))

(defun aero/npm-run-start (&optional comint)
  "Run start in COMINT."
  (interactive (list (consp current-prefix-arg)))
  (aero/npm--exec "start" comint))

(defun aero/npm-open-package-json ()
  "Open project's package.json."
  (interactive)
  (find-file (aero/npm--project-file)))

(aero-mode-leader-def
  :keymaps '(js-mode-map web-mode-map)
  "n" '(:ignore t :wk "npm")
  "nI" 'aero/npm-init
  "ni" 'aero/npm-install
  "nS" 'aero/npm-install-save
  "nd" 'aero/npm-install-save-dev
  "nu" 'aero/npm-uninstall
  "nl" 'aero/npm-list
  "nr" 'aero/npm-run
  "nt" 'aero/npm-run-test
  "ns" 'aero/npm-run-start
  "np" 'aero/npm-open-package-json
  "t" 'aero/npm-run-test
  "s" 'aero/npm-run-start
  "f" 'css-cycle-color-format)


(provide 'aero-web)

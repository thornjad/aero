;; -*- lexical-binding: t -*-
;;
;; web, js, cs, etc.
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

(use-package web-mode :straight t
  :load-path "lib/packages/web-mode/"
  :mode
  "\\.\\(tpl\\|php\\|xml\\|html?\\|djhtml\\|erb\\|eco\\|ejs\\)\\'")

(use-package emmet-mode :straight t
  :load-path "lib/packages/emmet-mode/"
  :hook ((web-mode html-mode css-mode scss-mode rjsx-mode) . emmet-mode)
	:init
	(setq emmet-self-closing-tag-style " /")

  :config
  (add-hook
   'rjsx-mode-hook
   (lambda () (setq emmet-expand-jsx-className? t))))

(use-package scss-mode :straight t
  :load-path "lib/packages/emmet-mode"
  :mode "\\.s?css\\'"
  :ensure-system-package
  (sass-lint . "npm i -g sass-lint"))


;; js and jsx

(use-package js2-mode
  :load-path "lib/packages/js2-mode/"
  :defer t)

(use-package rjsx-mode :straight t
  :after (js2-mode emmet evil)
  :mode "\\.jsx?\\'"

  :init/el-patch ; don't override indentation
  (defun rjsx--indent-line-1 ()
    "Helper for `rjsx-indent-line'."
    (let* ((el-patch-remove
             (indent-tabs-mode nil))
           (cur-pos (point))
           (cur-char (char-after cur-pos))
           (node (js2-node-at-point (- cur-pos rjsx--indent-running-offset)))
           (parent (js2-node-parent node)))
      (cond
       ((rjsx-node-p node)
        (cond
         ((eq cur-char ?<)
          (if (rjsx-node-p parent)
              (rjsx--indent-line-to-offset parent sgml-basic-offset)
            ;; Top-level node, indent as JS
            (js-indent-line))
          (when rjsx--node-abs-pos-cache
            (setf (gethash node rjsx--node-abs-pos-cache)
                  (save-excursion (back-to-indentation) (point)))))
         ((memq cur-char '(?/ ?>))
          (rjsx--indent-line-to-offset node 0))
         ((eq cur-char ?\n)
          (rjsx--indent-line-to-offset node sgml-basic-offset))
         (t (error "Don't know how to indent %s for JSX node" (make-string 1 cur-char)))))
       ((and (rjsx-identifier-p parent)
           (rjsx-member-p (js2-node-parent parent))
           (rjsx-node-p (js2-node-parent (js2-node-parent parent))))
        (rjsx--indent-line-to-offset (js2-node-parent (js2-node-parent parent)) 0))

       ;; JSX children
       ((rjsx-closing-tag-p node)
        (rjsx--indent-line-to-offset parent 0))
       ((rjsx-text-p node)
        (rjsx--indent-line-to-offset parent sgml-basic-offset))
       ((rjsx-wrapped-expr-p node)
        (if (eq cur-char ?})
            (js-indent-line)
          (rjsx--indent-line-to-offset parent sgml-basic-offset)))

       ;; Attribute-like (spreads, attributes, etc.)
       ;; if first attr is on same line as tag, then align
       ;; otherwise indent to parent level + sgml-basic-offset
       ((or (rjsx-identifier-p node)
           (and (rjsx-identifier-p parent)
              (rjsx-attr-p (js2-node-parent parent)))
           (rjsx-spread-p node))
        (let* ((tag (or (rjsx-ancestor node #'rjsx-node-p)
                       (error "Did not find containing JSX tag for attributes")))
               (name (rjsx-node-name tag))
               column)
          (save-excursion
            (goto-char (rjsx--node-abs-pos tag))
            (setq column (current-column))
            (when name (forward-char (js2-node-end name)) (skip-chars-forward " \t"))
            (if (eolp)
                (setq column (+ column sgml-basic-offset sgml-attribute-offset))
              (setq column (current-column))))
          (indent-line-to column)))

       ;; Everything else indent as javascript
       (t (js-indent-line)))

      (when rjsx--indent-region-p
        (cl-incf rjsx--indent-running-offset
                 (- (save-excursion (back-to-indentation) (point))
                    cur-pos)))))

  ;; ensure flycheck can run properly
  :ensure-system-package
  ((eslint . "npm i -g eslint")
   (tern . "npm i -g tern"))

  :config
  (add-to-list
   'load-path
   "/Users/jade.thornton/.nvm/versions/node/v13.1.0/lib/node_modules/tern/emacs/")
  (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'rjsx-mode #'tern-mode)
  (eval-when-compile (defvar emmet-expand-jsx-className?)) ; defined in rjsx-mode
  (add-hook 'rjsx-mode-hook (lambda () (setq emmet-expand-jsx-className? t)))
  ;; FIXME something is resetting this when this mode loads, need to find out how/where
  (setq js2-basic-offset 2)

  ;; because we want C-d to scroll ups normally
  (evil-define-key 'insert rjsx-mode-map
    (kbd "C-d") 'rjsx-delete-creates-full-tag)
  (evil-define-key 'normal rjsx-mode-map
    (kbd "C-d") 'evil-scroll-down))

(use-package json-mode :straight t
	:mode "\\.json\\'")

(use-package yaml-mode :straight t
  :mode "\\.ya?ml\\'")

;; the rest

(use-package coffee-mode :straight t
  :mode "\\.coffee\\'"
  :ensure-system-package
  ((coffee . "npm i -g coffeescript")
   (coffeelint . "npm i -g coffeelint")))

(provide 'aero-web)

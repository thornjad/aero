;; rivet-mode.el -- major mode for editing Apache Rivet files

;; Copyright 2019 Jade Michael Thornton

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; See the included <LICENSE> file for details.

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Commentary

;; Rivet mode supports font-lock, indentation and all the other goodies for
;; Apache Rivet files. It does this by intelligently (allegedly) switching
;; between TCL and HTML modes.

;; The structure of Rivet mode is based on two-mode-mode, developed in 1999 by
;; David N. Welton and the Apache Software Foundation.

(defvar default-mode (list "Web" 'web-mode))
;; TODO need to support inline <?= mode
(defvar second-mode (list "Tcl" "<?" "?>" 'tcl-mode))

(defvar rivet-update 0)
(defvar rivet-mode-idle-timer nil)
(defvar rivet-bool nil)
(defvar rivet-mode-delay (/ (float 3) (float 8)))

;; Rivet mode hook
(defvar rivet-hook nil
  "*Hook called by `rivet-mode'.")
(setq rivet-hook nil)

;; Mode switching hook
(defvar rivet-switch-hook nil
  "*Hook called upon mode switching.")
(setq rivet-switch-hook nil)

(defun rivet-mode-setup ()
  (add-hook 'post-command-hook 'rivet-mode-need-update nil t)
  (make-local-variable 'minor-mode-alist)
  (make-local-variable 'rivet-bool)
  (setq rivet-bool t)
  (when rivet-mode-idle-timer
    (cancel-timer rivet-mode-idle-timer))
  (setq rivet-mode-idle-timer
				(run-with-idle-timer rivet-mode-delay t
														 'rivet-mode-update-mode))
  (or (assq 'rivet-bool minor-mode-alist)
      (setq minor-mode-alist
						(cons '(rivet-bool " rivet-mode") minor-mode-alist))))

(defun rivet-mode-need-update ()
  (setq rivet-update 1))

(defun rivet-change-mode (to-mode func)
  (if (string= to-mode mode-name)
      t
    (progn
      (funcall func)
      ;; After the mode was set, we reread the "Local Variables" section.
      ;; We do need this for example in SGML-mode if "sgml-parent-document"
      ;; was set, or otherwise it will be reset to nil when sgml-mode is left.
      (hack-local-variables)

      (rivet-mode-setup)
      (if rivet-switch-hook
					(run-hooks 'rivet-switch-hook))
      (if (eq font-lock-mode t)
					(font-lock-fontify-buffer))
			(turn-on-font-lock-if-desired))))

(defun rivet-mode-update-mode ()
  (when (and rivet-bool rivet-update)
    (setq rivet-update 0)
		(let ((flag 0)
					(mode second-mode)
					(lm -1)
					(rm -1))
			(save-excursion
				(if (search-backward (cadr mode) nil t)
						(setq lm (point))
					(setq lm -1)))
			(save-excursion
				(if (search-backward (car (cddr mode)) nil t)
						(setq rm (point))
					(setq rm -1)))
			(if (and (not (and (= lm -1) (= rm -1))) (>= lm rm))
					(progn
						(setq flag 1)
						(rivet-change-mode (car mode) (car (cdr (cddr mode))))))
			(if (= flag 0)
					(rivet-change-mode (car default-mode) (cadr default-mode))))))

;;;###autoload
(defun rivet-mode ()
  "Turn on rivet-mode"
  (interactive)
  (funcall (cadr default-mode))
  (rivet-mode-setup)
  (if rivet-hook
			(run-hooks 'rivet-hook)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rvt$" . rivet-mode))

(provide 'rivet-mode)

;;; rivet-mode.el ends here

;;; aero-breadcrumb.el --- imenu-based aero-breadcrumb paths   -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jade Michael Thornton
;; Copyright (C) 2023 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;;; Usage:
;;
;; Aero/Aero-Breadcrumbs are sequences of short strings indicating where you
;; are in some big tree-like maze.
;;
;; To use this library:
;;
;; * `M-x aero/aero-breadcrumb-local-mode` is a buffer-local minor mode
;;
;;;; Implementation notes:
;;
;; Aero/Aero-Breadcrumb uses the double-dashed Imenu symbols
;; `imenu--index-alist' and `imenu--make-index-alist'.  There's
;; really no official API here.  It's arguable that, despite the
;; name, these aren't really internal symbols (the much older
;; which-func.el library makes liberal use of them, for example).
;;

;;; Code:
(require 'cl-lib)
(require 'imenu)


;;;; Customization options

(defgroup aero-breadcrumb nil
  "One-liner indication of where you are in the maze."
  :prefix "aero-breadcrumb-"
  :group 'convenience)

(defcustom aero-breadcrumb-max-length 0.98
  "Soft cutoff for `aero-breadcrumb-crumbs'.
If a fixnum, it's a absolute number of characters.  If a float, a
percentage of `window-width'."
  :type '(choice (natnum :tag "Number of characters")
                 (float  :tag "Percent of window's width")))

(defcustom aero-breadcrumb-crumb-separator " > "
  "Separator for `aero-breadcrumb-crumbs'." :type 'string)

(defface aero-breadcrumb-face '((t (:inherit shadow)))
  "Base face for all aero-breadcrumb things.")

(defface aero-breadcrumb-crumbs-face '((t (:inherit aero-breadcrumb-face)))
  "Face for imenu crumbs in the aero-breadcrumb imenu path.")

(defface aero-breadcrumb-leaf-face '((t (:inherit (font-lock-function-name-face
                                                   aero-breadcrumb-crumbs-face))))
  "Face for imenu leaf crumbs in the aero-breadcrumb imenu path.")


;;;; "ipath" management logic and imenu interoperation

(cl-defun aero-breadcrumb--bisect (a x &key (from 0) (to (length a)) key from-end)
  "Compute index to insert X in sequence A, keeping it sorted.
If X already in A, the resulting index is the leftmost such
index, unless FROM-END is t.  KEY is as usual in other CL land."
  (cl-macrolet ((search (from-end key)
                        `(cl-loop while (< from to)
                                  for mid = (/ (+ from to) 2)
                                  for p1 = (elt a mid)
                                  for p2 = ,(if key `(funcall key p1) `p1)
                                  if (,(if from-end '< '<=) x p2)
                                  do (setq to mid) else do (setq from (1+ mid))
                                  finally return from)))
    (if from-end (if key (search t key) (search t nil))
      (if key (search nil key) (search nil nil)))))

(defun aero-breadcrumb--ipath-rich (index-alist pos)
  "Compute ipath for rich `imenu--index-alist' structures.
These structures have a `aero-breadcrumb-region' property on every
node."
  (cl-labels
      ((search (nodes &optional ipath)
               (cl-loop
                for n in nodes
                for reg = (get-text-property 0 'aero-breadcrumb-region (car n))
                when (<= (car reg) pos (cdr reg))
                return (search (cdr n) (cons
                                        (propertize (car n)
                                                    'aero-breadcrumb-siblings nodes
                                                    'aero-breadcrumb-parent (car ipath))
                                        ipath))
                finally (cl-return ipath))))
    (nreverse (search index-alist))))

(defvar-local aero-breadcrumb--ipath-plain-cache nil
  "A cache for `aero-breadcrumb--ipath-plain'.")

(defun aero-breadcrumb--ipath-plain (index-alist pos)
  "Compute ipath for plain `imenu--index-alist' structures.
These structures don't have a `aero-breadcrumb-region' property on."
  (cl-labels ((dfs (n &optional ipath siblings)
                   (setq ipath (cons (car n) ipath))
                   (if (consp (cdr n))
                       (mapc (lambda (n2) (dfs n2 ipath (cdr n))) (cdr n))
                     ;; FIXME: we convert markers to points via the `+'
                     ;; down there.  But for siblings, no such conversion
                     ;; happens, they might still point to invalid
                     ;; markers.  Not worth doing another tree traversal
                     ;; for that IMO, and siblings seems to be unused
                     ;; anyway (github#22)
                     (put-text-property 0 1 'aero-breadcrumb-siblings (cdr siblings) (car ipath))
                     (setq aero-breadcrumb--ipath-plain-cache
                           (vconcat aero-breadcrumb--ipath-plain-cache
                                    `[,(cons
                                        ;; See github#17 and docstring of
                                        ;; `imenu--index-alist' for the
                                        ;; "overlay" edge case.
                                        (cl-etypecase (cdr n)
                                          (number (cdr n))
                                          (marker (+ (cdr n) 0))
                                          (overlay (overlay-start (cdr n))))
                                        ipath)])))))
    (unless aero-breadcrumb--ipath-plain-cache
      (mapc (lambda (i) (dfs i nil index-alist)) index-alist)
      (setq aero-breadcrumb--ipath-plain-cache (cl-sort aero-breadcrumb--ipath-plain-cache #'< :key #'car)))
    (unless (< pos (car (aref aero-breadcrumb--ipath-plain-cache 0)))
      (let ((res (aero-breadcrumb--bisect aero-breadcrumb--ipath-plain-cache pos :key #'car :from-end t)))
        (unless (zerop res) (reverse (cdr (elt aero-breadcrumb--ipath-plain-cache (1- res)))))))))

(defun aero-breadcrumb-ipath (index-alist pos)
  "Get aero-breadcrumb for position POS given INDEX-ALIST."
  (if (get-text-property 0 'aero-breadcrumb-region (caar index-alist))
      (aero-breadcrumb--ipath-rich index-alist pos)
    (aero-breadcrumb--ipath-plain index-alist pos)))

(defvar aero-breadcrumb-idle-time 1
  "Control idle time before requesting new aero-breadcrumbs.")

(defvar-local aero-breadcrumb--idle-timer nil
  "Timer used by `aero-breadcrumb--ipath-alist'.")

(defvar-local aero-breadcrumb--last-update-tick 0
  "Last time `aero-breadcrumb--ipath-alist' asked for an update.")

(defun aero-breadcrumb--ipath-alist ()
  "Return `imenu--index-alist', maybe arrange for its update."
  (let ((nochangep (= (buffer-chars-modified-tick) aero-breadcrumb--last-update-tick))
        (buf (current-buffer)))
    (unless nochangep
      (setq aero-breadcrumb--last-update-tick (buffer-chars-modified-tick))
      (when aero-breadcrumb--idle-timer (cancel-timer aero-breadcrumb--idle-timer))
      (setq aero-breadcrumb--idle-timer
            (run-with-idle-timer
             aero-breadcrumb-idle-time nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq aero-breadcrumb--last-update-tick (buffer-chars-modified-tick))
                   (let ((non-essential t)
                         (imenu-auto-rescan t))
                     (ignore-errors
                       (imenu--make-index-alist t))
                     (setq aero-breadcrumb--ipath-plain-cache nil)
                     ;; no point is taxing the mode-line machinery now
                     ;; if the buffer isn't showing anywhere.
                     (when (get-buffer-window buf t)
                       (force-mode-line-update t)))))))))
    imenu--index-alist))


;;;; Higher-level functions

(defun aero-breadcrumb--length (len)
  "Interpret LEN using `window-width' and return a number."
  (cond ((floatp len) (* (window-width) len))
        (t len)))

(defun aero-breadcrumb--format-ipath-node (p more)
  (propertize p 'face (if more 'aero-breadcrumb-crumbs-face 'aero-breadcrumb-leaf-face)
              'aero-breadcrumb-dont-shorten (null more)))

(defun aero-breadcrumb-crumbs ()
  "Describe point inside the Imenu tree of current file."
  (when-let ((alist (aero-breadcrumb--ipath-alist)))
    (when (cl-some #'identity alist)
      (aero-breadcrumb--summarize
       (cl-loop
        for (p . more) on (aero-breadcrumb-ipath alist (point))
        collect (aero-breadcrumb--format-ipath-node p more))
       (aero-breadcrumb--length aero-breadcrumb-max-length)
       (propertize aero-breadcrumb-crumb-separator
                   'face 'aero-breadcrumb-face)))))

(defun aero-breadcrumb--summarize (crumbs cutoff separator)
  "Return a string that summarizes CRUMBS, a list of strings.
\"Summarization\" consists of truncating some CRUMBS to 1
character.  Rightmost members of CRUMBS are summarized last.
Members with a `aero-breadcrumb-dont-shorten' are never truncated.
Aim for a return string that is at most CUTOFF characters long.
Join the crumbs with SEPARATOR."
  (let ((rcrumbs
         (cl-loop
          for available = (- cutoff used)
          for (c . more) on (reverse crumbs)
          for seplen = (if more (length separator) 0)
          for shorten-p = (unless (get-text-property 0 'aero-breadcrumb-dont-shorten c)
                            (> (+ (length c) seplen) available))
          for toadd = (if shorten-p (substring c 0 1) c)
          sum (+ (length toadd) seplen) into used
          collect toadd)))
    (string-join (reverse rcrumbs) separator)))

(defun aero-breadcrumb--header-line ()
  "Helper for `aero-breadcrumb-headerline-mode'."
  (let ((x (cl-remove-if #'seq-empty-p (mapcar #'funcall '(aero-breadcrumb-crumbs)))))
    (mapconcat #'identity x (propertize " : " 'face 'aero-breadcrumb-face))))

;;;###autoload
(define-minor-mode aero-breadcrumb-local-mode
  "Header lines with aero-breadcrumbs."
  :init-value nil
  (if aero-breadcrumb-local-mode (add-to-list 'header-line-format '(:eval (aero-breadcrumb--header-line)))
    (setq header-line-format (delete '(:eval (aero-breadcrumb--header-line)) header-line-format))))

(provide 'aero-breadcrumb)
;;; aero-breadcrumb.el ends here

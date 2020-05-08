;; -*- lexical-binding: t -*-
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
;; Commentary:
;;
;; A home for utilities

(require 'cl-lib)

;; Code:


;;; utilities

;; Load in external libs
(use-package memoize
  :straight (memoize :host gitlab :repo "thornjad/memoize"))

(defun aero/applescript-escape (str)
  "Escape STR to make it suitable for using is applescripts."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defmacro aero/defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook. ARGLIST is as in `defun'.
HOOKS is a list of hooks to which to add the function, or just a single hook.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (aero/log-error "symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (aero/log-error "no docstring provided for `aero/defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

(defun aero/guess-startup-directory-using-proc ()
  "Get the startup directory of current Emacs session from /proc."
  (when (file-exists-p (format "/proc/%d/cwd" (emacs-pid)))
    (file-chase-links (format "/proc/%d/cwd" (emacs-pid)))))

(defun aero/guess-startup-directory-using-lsof ()
  "Get the startup directory of the current Emacs session using`lsof'."
  (when (executable-find "lsof")
    (let* ((default-directory "/")
           (lsof-op (shell-command-to-string (format "lsof -d cwd -a -Fn -p %d"
                                                     (emacs-pid))))
           (raw-cwd (car (last (split-string lsof-op "\n" t))))
           (cwd (substring raw-cwd 1)))
      (when (< 0 (length cwd))
        cwd))))

(defun aero/guess-startup-directory-using-buffers ()
  "Guess the startup directory for current Emacs session from some buffer.
This tries to get Emacs startup directory from the *Messages* or *scratch*
buffer, needless to say this would be wrong if the user has killed and recreated
these buffers."
  (or (and (get-buffer "*Messages*")
           (with-current-buffer "*Messages*" default-directory))
      (and (get-buffer "*scratch*")
           (with-current-buffer "*scratch*" default-directory))))

(defun aero/guess-startup-directory ()
  "Guess the directory the new Emacs instance should start from.
On Linux it figures out the startup directory by reading /proc entry for current
Emacs instance. Otherwise it falls back to guessing the startup directory by
reading `default-directory' of *Messages* or *scratch* buffers falling back to
the HOME environment variable and finally just using whatever is the current
`default-directory'."
  (or (aero/guess-startup-directory-using-proc)
      (aero/guess-startup-directory-using-lsof)
      (aero/guess-startup-directory-using-buffers)
      (getenv "HOME")
      default-directory))

(defmacro aero/defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function. ARGLIST is as in
`defun'. WHERE is a keyword as passed to `advice-add', and PLACE is the function
to which to add the advice, like in `advice-add'. DOCSTRING and BODY are as in
`defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (aero/log-error "no docstring provided for `aero/defadvice'"))
  `(progn
     (eval-and-compile
       (defun ,name ,arglist
         ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                             "an"
                           "a")))
            (format "%s\n\nThis is %s `%S' advice for `%S'."
                    docstring article where
                    (if (and (listp place)
                             (memq (car place) ''function))
                        (cadr place)
                      place)))
         ,@body))
     (advice-add ',place ',where #',name)
     ',name))

(defun aero/path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of SEGMENTS, with PATH as
DEFAULT-DIRECTORY. Then `expand-file-name' is called on the second member, with
the result of the first call as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are
passed, the return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

(defun aero/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the
sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


;;; system and logging

(defun system-is-mac ()
  (eq system-type 'darwin))
(defun system-is-linux ()
  (eq system-type 'gnu/linux))
(defun system-is-mswindows ()
  (eq system-type 'windows-nt))
(defun window-system-is-mac ()
  (memq (window-system) '(mac ns)))

(defun aero/log-error (msg &rest args)
  "Display MSG as an error message in `*Messages*' buffer"
  (let ((msg (apply 'format msg args)))
    (message "(aero) Error: %s" msg)))

(defun aero/log-warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'"
  (let ((msg (apply 'format msg args)))
    (message "(aero) Warning: %s" msg)))

(defun aero/log-info (msg &rest args)
	"Display MSG as an info message in buffer `*Messages'"
	(let ((msg (apply 'format msg args)))
		(message "(aero) Info: %s" msg)))

(defun aero/echo (msg &rest args)
  "Display MSG in echo-area without logging it in `*Messages' buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))


;; program-wide

;; https://sachachua.com/blog/2006/09/emacs-changing-the-font-size-on-the-fly/
(defun aero/increase-font-size ()
  (interactive)
  (set-face-attribute
	 'default
   nil
   :height
   (ceiling (* 1.10
               (face-attribute 'default :height)))))
(defun aero/decrease-font-size ()
  (interactive)
  (set-face-attribute
	 'default
   nil
   :height
   (floor (* 0.9
             (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'aero/increase-font-size)
(global-set-key (kbd "C--") 'aero/decrease-font-size)

(defun aero/apologize-to-emacs ()
	"Apologize for smacking emacs (stop debug on quit). Use this after smacking
emacs with sigusr2"
	(interactive)
	(toggle-debug-on-quit))

(defun aero/thornlog ()
	"Personal persistent log."
	(interactive)
	(find-file (expand-file-name "~/doc/thornlog.org")))

(defun aero/eshell-taskwarrior ()
  "Open a specialized eshell buffer for taskwarrior."
  (interactive)
  (when (require 'eshell nil 'no-error)
    (declare-function eshell-return-to-prompt "eshell.el")
    (declare-function eshell-send-input "eshell.el")
    (declare)
    (let ((default-directory (expand-file-name "~")))
      (switch-to-buffer (get-buffer-create "*aero taskwarrior*"))
      (eshell-mode)
      (eshell-return-to-prompt)
      (insert "task")
      (eshell-send-input))))
(defalias 'aero/task #'aero/eshell-taskwarrior)

(defun aero/stop-auto-revert-buffers ()
	(cancel-function-timers 'auto-revert-buffers))


;; buffers, windows, frames, tabs

(defun aero/toggle-prettify-this-buffer ()
  "Disable `prettify-symbols-mode' in this buffer."
  (interactive)
  (if prettify-symbols-mode
      (prettify-symbols-mode -1)
    (prettify-symbols-mode 1)))

(defun aero/switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun switch-to-messages-buffer ()
	(interactive)
	(switch-to-buffer "*Messages*"))

(defun switch-to-scratch-buffer ()
	(interactive)
	(switch-to-buffer "*Scratch*"))

;; from spacemacs
(defun aero/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (cl-destructuring-bind
      (buf start pos)
      (or (cl-find (window-buffer window) (window-prev-buffers)
                   :key #'car :test-not #'eq)
          (list (other-buffer) nil nil ))
    (set-window-buffer-start-and-point window buf start pos)))

(defun aero/alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

(defun aero/layout-two-columns ()
  "Switch to two column window layout."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(defun aero/layout-three-columns ()
  "Switch to three column window layout."
  (interactive)
  (delete-other-windows)
  (dotimes (_ 2) (split-window-right))
  (balance-windows))

(defun aero/eshell-new ()
  "Open a new Eshell window.

This is equivalent to SPC U M-x eshell"
  (interactive)
  (eshell t))

(defun make-xpm-bar (color height width)
  "Create an XPM bar bitmap of HEIGHT and WIDTH, with COLOR accent."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))


;;; files

(defun aero/reopen-file-at-buffer ()
  "Re-open the file at buffer, replacing buffer.

After reopening, cursor will attempt to return to the point it was previously
on. This may cause a jump if the file has changed significantly."
  (interactive)
  (let ((initial-line (line-beginning-position))
        (initial-point (point))
        (initial-total-lines (count-lines (point-min) (point-max))))
    (find-alternate-file (buffer-file-name))
    ;; TODO this calculation does not always seem reliable
    (if (= initial-total-lines (count-lines (point-min) (point-max)))
        ;; If total lines have not changed, we can reasonably guess that the
        ;; content has not changed significantly (if at all), so we can jump
        ;; right back to the initial point.
        (goto-char initial-point)
      ;; If total lines /have/ changed, we can reasonably guess that the initial
      ;; point is contextually not where we were before. The best thing we can
      ;; do now is return to the same line number, and hope it's close. Getting
      ;; closer than this would require text parsing, which is more complex than
      ;; we need for a simple file replacement.
      (goto-char initial-line))))

(defun aero/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun aero/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun aero/buffer-too-big-p ()
	"Is buffer longer than 5000"
  (> (buffer-size) (* 5000 80)))

(defun aero/file-too-big-p (file)
	"Is file larger than 5000 lines"
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

;; adapted from
;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
(defun aero/kill-diff-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (member (buffer-local-value 'major-mode buffer)
                        '(diff-mode magit-diff-mode magit-process-mode))
            (kill-buffer buffer)))
        (buffer-list)))

(defun aero/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun aero/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun aero/run-osascript (script-content)
  "Run an SCRIPT-CONTENT as AppleScript/osascipt."
  (interactive "sContent of AppleScript/osascript:")
  (let ((file (make-temp-file "aero-temp-osa-" nil ".applescript")))
    (with-temp-file file
      (insert script-content)
      (insert "\ndo shell script \"rm -rf \" & the quoted form of POSIX path of (path to me)"))
    (aero/run-osascript-file file)))
(defalias 'aero/run-applescript #'aero/run-osascript)

(defun aero/run-osascript-file (file)
  "Run an AppleScript/osascipt FILE."
  (with-current-buffer (get-buffer-create "*OsaScript*")
    (insert "Going to run file: " file))
  (start-process "OsaScript" "*OsaScript*" "osascript" file))
(defalias 'aero/run-applescript-file #'aero/run-osascript-file)

(defun aero/osx-notify2 (title message)
  "Create a notification with TITLE and MESSAGE."
  (aero/run-osascript
   (concat "display notification \""
	         (aero/applescript-escape message)
	         "\" with title  \""
	         (aero/applescript-escape title)
	         "\"")))

(defun aero/osx-notify3 (title subtitle message)
  "Create a notification with TITLE, SUBTITLE and MESSAGE."
  (aero/run-osascript
   (concat "display notification \""
	         (aero/applescript-escape message)
	         "\" with title  \""
	         (aero/applescript-escape title)
	         "\" subtitle \""
	         (aero/applescript-escape subtitle)
	         "\"")))

(defun aero/osx-beep ()
  "Play beep sound."
  (aero/run-applescript "beep"))

(defun aero/reveal-in-finder-as (file)
  "Reveal the supplied file FILE in Finder.

To call interactively, use [aero/open-in-finder]."
  (let ((script (concat
                 "set thePath to POSIX file \"" (shell-quote-argument file) "\"\n"
                 "tell application \"Finder\"\n"
                 " set frontmost to true\n"
                 " reveal thePath \n"
                 "end tell\n")))
    (aero/run-osascript script)))

(defun aero/open-in-finder ()
  "Reveal the file associated with the current buffer in the OSX Finder.
In a dired buffer, it will open the current file."
  (interactive)
  (aero/reveal-in-finder-as
   (or (buffer-file-name)
       (expand-file-name (or (dired-file-name-at-point) ".")))))

(defun aero/sudo-edit (&optional arg)
  (interactive "P")
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (cond ((string-match-p "^/ssh:" fname)
            (with-temp-buffer
              (insert fname)
              (search-backward ":")
              (let ((last-match-end nil)
                    (last-ssh-hostname nil))
                (while (string-match "@\\\([^:|]+\\\)" fname last-match-end)
                  (setq last-ssh-hostname (or (match-string 1 fname)
                                              last-ssh-hostname))
                  (setq last-match-end (match-end 0)))
                (insert (format "|sudo:%s" (or last-ssh-hostname "localhost"))))
              (buffer-string)))
           (t (concat "/sudo:root@localhost:" fname))))))

(defun aero/open-local-init ()
  "Open local init file for editing."
  (interactive)
  (find-file (concat user-emacs-directory "init.local.el")))
(defun aero/open-emacs-problems ()
  "Open Emacs PROBLEMS file from GitHub mirror."
  (interactive)
  (eww "https://github.com/emacs-mirror/emacs/blob/master/etc/PROBLEMS"))

(defun aero/restclient-scratch ()
  "Open a restclient scratch buffer."
  (interactive)
  (when (require 'restclient nil 'noerror)
    (switch-to-buffer (get-buffer-create "restclient-scratch"))
    (insert "# -*- restclient -*-\n")
    (insert "# File is not saved, use the comma (,) prefix menu for actions\n\n")
    (restclient-mode)))

;;; editing et cetera

;; written by github user rompy
(defun aero/smarter-backward-kill-word ()
  "Deletes the previous word, respecting:
1. If the cursor is at the beginning of line, delete the '\n'.
2. If there is only whitespace, delete only to beginning of line.
3. If there is whitespace, delete whitespace and check 4-5.
4. If there are other characters instead of words, delete one only char.
5. If it's a word at point, delete it."
  (interactive)
  (if (bolp)
      (delete-char -1)
    (if (string-match-p "^[[:space:]]+$"
                        (buffer-substring-no-properties
                         (line-beginning-position) (point)))
        (delete-horizontal-space)
      (when (thing-at-point 'whitespace)
        (delete-horizontal-space))
      (if (thing-at-point 'word)
          (let ((start (car (bounds-of-thing-at-point 'word)))
                (end (point)))
            (if (> end start)
                (delete-region start end)
              (delete-char -1)))
        (delete-char -1)))))

(defun aero/go-to-tag (arg)
  "Go to tag under point.
If called with prefix argument, or with nothing under point, prompt for tag."
  (interactive "P")
  (when (fboundp 'xref-find-definitions)
    (let ((xref-prompt-for-identifier arg))
      (call-interactively #'xref-find-definitions))))

(defun aero/byte-compile-file-at-buffer ()
  "Byte compile the file open in the current buffer."
  (interactive)
  (save-excursion
    (byte-compile-file buffer-file-name)))
(defun aero/byte-recompile-file-at-buffer ()
  "Byte recompile the file open in the current buffer."
  (interactive)
  (save-excursion
    (byte-recompile-file buffer-file-name)))

(defun shrug ()
	(interactive)
	(insert "¯\\_(ツ)_/¯"))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun insert-lambda ()
  (interactive)
  (save-excursion
    (right-char)
    (insert "λ")))

(defun alter-number-at-point (offset)
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (aero/log-error "No number at point"))
    (replace-match (number-to-string (+ offset (string-to-number (match-string 0)))))))

(defun increment-number-at-point ()
  (interactive)
  (alter-number-at-point 1))

(defun decrement-number-at-point ()
  (interactive)
  (alter-number-at-point -1))

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Praesent libero orci, auctor sed, faucibus vestibulum, gravida vitae, arcu. Nunc posuere. Suspendisse potenti. Praesent in arcu ac nisl ultricies ultricies. Fusce eros. Sed pulvinar vehicula ante. Maecenas urna dolor, egestas vel, tristique et, porta eu, leo. Curabitur vitae sem eget arcu laoreet vulputate. Cras orci neque, faucibus et, rhoncus ac, venenatis ac, magna. Aenean eu lacus. Aliquam luctus facilisis augue. Nullam fringilla consectetuer sapien. Aenean neque augue, bibendum a, feugiat id, lobortis vel, nunc. Suspendisse in nibh quis erat condimentum pretium. Vestibulum tempor odio et leo. Sed sodales vestibulum justo. Cras convallis pellentesque augue. In eu magna. In pede turpis, feugiat pulvinar, sodales eget, bibendum consectetuer, magna. Pellentesque vitae augue."))

(provide 'aero-lib)

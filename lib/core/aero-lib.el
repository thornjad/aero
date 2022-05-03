;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2022 Jade Michael Thornton
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
;; A home for utilities

(require 'cl-lib)
(require 'use-package)

;;; Code:

;; Requirements
(use-package memo :straight (:host gitlab :repo "thornjad/emacs-memo" :branch "main"))
(use-package async :straight (:host github :repo "jwiegley/emacs-async") :commands (async-save))
(use-package popup :straight t)
(require 'aero-pbcopier) ; Can be found in this dir

;; basic helpers
(defun rand-nth (coll)
  "Return a random element of the list COLL."
  (nth (random (length coll)) coll))


;; utilities

(defun aero/keyboard-quit-context ()
  "Quit current context.

This function is a combination of `keyboard-quit' and `keyboard-escape-quit'
with some parts omitted and some custom behavior added."
  ;; Adapted from https://with-emacs.com/posts/tips/quit-current-context/
  (interactive)
  (cond
   ((region-active-p)
    ;; Avoid adding the region to the window selection.
    (setq saved-region-selection nil)
    (let (select-active-regions)
      (deactivate-mark)))

   ((eq last-command 'mode-exited) nil)

   (current-prefix-arg nil)

   (defining-kbd-macro
     (message
      (substitute-command-keys
       "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
     (cancel-kbd-macro-events))

   ((active-minibuffer-window)
    (when (get-buffer-window "*Completions*")
      ;; hide completions first so point stays in active window when
      ;; outside the minibuffer
      (minibuffer-hide-completions))
    (abort-recursive-edit))

   (t (keyboard-quit))))

(defun aero/applescript-escape (str)
  "Escape STR to make it suitable for using is applescripts."
  (replace-regexp-in-string "\"" "\\\\\"" str))

(defmacro aero/local! (&rest body)
  "Execute BODY in local directory instead of TRAMP."
  `(let ((default-directory user-emacs-directory)) ,@body))

(defmacro aero/voidvar! (&rest body)
  "Appease the compiler by pretending to use variables in BODY.

Similar to C++'s void var construct."
  `(and ,@body))

(defun aero/sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.

The variable `sort-fold-case' determines whether alphabetic case affects the
sort order.

See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" beg end))


;;; system and logging

(defun system-is-mac () (string= system-type 'darwin))
(defun system-is-linux () (string= system-type 'gnu/linux))
(defun system-is-mswindows () (string= system-type 'windows-nt))
(defun window-system-is-mac () (memq (window-system) '(mac ns)))
(defun in-nix-shell-p () (string-equal (getenv "IN_NIX_SHELL") "1"))


;; program-wide

;; https://sachachua.com/blog/2006/09/emacs-changing-the-font-size-on-the-fly/
(defun aero/increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height (ceiling (* 1.10 (face-attribute 'default :height)))))
(defun aero/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height (floor (* 0.9 (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'aero/increase-font-size)
(global-set-key (kbd "C--") 'aero/decrease-font-size)


;; buffers, windows, frames, tabs

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

(defun switch-to-new-scratch-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Scratch*")))

(defun aero/bury-buffer-kill-window (&optional window)
  "Bury the current buffer and kill its window, or use WINDOW."
  (interactive)
  (let* ((buf (window-buffer window))
         (win (get-buffer-window buf)))
    (bury-buffer buf)
    (delete-window win)))

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

(defun aero/toggle-compilation-buffer ()
  "Pop-up the compilation buffer."
  (interactive)
  (aero/toggle-popup-buffer "*compilation*"))

(defun aero/toggle-popup-buffer (buf)
  "Pop-up BUF in a buffer below."
  (let ((win (get-buffer-window buf 0)))
    (if win
        ;; found, so close it
        (aero/bury-buffer-kill-window win)

      ;; else we need to pop it up
      (display-buffer buf '((display-buffer-below-selected)
                            (reusable-frames . nil) ;; only search this frame
                            (window-height . 20))))))

(defun aero/incr-compilation-buffer ()
  "Renames existing compilation buffer so you can create more."
  (interactive)
  (let ((cbuf (get-buffer "*compilation*"))
        (more-cbufs t)
        (n 1)
        (new-cbuf-name ""))
    (when cbuf
      (while more-cbufs
        (setq new-cbuf-name (format "*compilation%d*" n))
        (setq n (1+ n))
        (setq more-cbufs (get-buffer new-cbuf-name)))
      (with-current-buffer cbuf
        (rename-buffer new-cbuf-name)))))

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
        (setf (point) initial-point)
      ;; If total lines /have/ changed, we can reasonably guess that the initial
      ;; point is contextually not where we were before. The best thing we can
      ;; do now is return to the same line number, and hope it's close. Getting
      ;; closer than this would require text parsing, which is more complex than
      ;; we need for a simple file replacement.
      (setf (point) initial-line))))

(defun aero/insert-date ()
  "Insert current date."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
(defun aero/insert-timestamp ()
  "Insert current timestamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M:%S")))

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

;; adapted from http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
(defun aero/kill-diff-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (member (buffer-local-value 'major-mode buffer)
                        '(diff-mode magit-diff-mode magit-process-mode))
            (kill-buffer buffer)))
        (buffer-list)))

(defun aero/fill-to-80 ()
  "`fill-paragraph' to 80 columns, regardless of the default."
  (interactive)
  (let ((fill-column 80))
    (fill-paragraph)))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

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
  (declare-function dired-file-name-at-point "dired.el")
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

(declare-function tramp-cleanup-all-connections "tramp.el")
(defun aero/tramp-buffer-p (buffer)
  (let ((name (buffer-name buffer)))
    (string-match "^\\*tramp" name)))
(defun aero/kill-tramp ()
  "Kill all Tramp connections. Useful for stale connections.
This function does NOT remove remote buffers, only their connections."
  (interactive)
  (when (require 'tramp nil t)
    (declare-function password-reset "password-cache.el")
    (password-reset)
    (cancel-function-timers 'tramp-timeout-session)
    (declare-function tramp-list-tramp-buffers "tramp.el")
    (dolist (name (tramp-list-tramp-buffers))
      (when (processp (get-buffer-process name)) (delete-process name)))))

(defun aero/kill-tags ()
  "Kill the currently-loaded TAGS file."
  (interactive)
  (when (get-buffer "TAGS")
    (kill-buffer "TAGS")))

(defun aero/open-local-init ()
  "Open local init file for editing."
  (interactive)
  (find-file (concat user-emacs-directory "init-local.el")))

(defun aero/open-emacs-problems ()
  "Open Emacs PROBLEMS file from GitHub mirror."
  (interactive)
  (eww "https://github.com/emacs-mirror/emacs/blob/master/etc/PROBLEMS"))

(defun aero/xdg-open (arg)
  "Pass the specified ARG to \"xdg-open\".
This can be used to open Nautilus/Finder, the default browser, etc. See \"man
xdg-open\" for more."
  (interactive (list (read-string "xdg-open: ")))
  (call-process "xdg-open" nil 0 nil arg))


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
      (aero/voidvar! xref-prompt-for-identifier)
      (call-interactively #'xref-find-definitions))))

(defun aero/native-compile-file-at-buffer ()
  "Native compile the file in the current buffer."
  (interactive)
  (let ((warning-minimum-level :warning))
    (save-excursion
      (native-compile-async buffer-file-name nil t))))

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

(defun untabify-buffer () (interactive) (untabify (point-min) (point-max)))
(defun tabify-buffer () (interactive) (tabify (point-min) (point-max)))
(defun indent-buffer () (interactive) (indent-region (point-min) (point-max)))

(defmacro aero/insert-text-at-point (text)
  `(progn
     (save-excursion
       (unless (eobp) (right-char))
       (insert ,text))
     (forward-sexp 1)))

(defun alter-number-at-point (offset)
  (save-excursion
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (message "No number at point"))
    (replace-match (number-to-string (+ offset (string-to-number (match-string 0)))))))
(defun increment-number-at-point () (interactive) (alter-number-at-point 1))
(defun decrement-number-at-point () (interactive) (alter-number-at-point -1))

(defun human-date (human-string &optional epoch)
  "Convert HUMAN-STRING to a date string or if EPOCH, seconds.
Requires the utility date to be installed."
  (with-temp-buffer
    (let ((dateProc (if (system-is-mac) "gdate" "date")))
      (if epoch
          (call-process dateProc nil t nil "-d" human-string "+%s")
        (call-process dateProc nil t nil "-d" human-string)))
    (replace-regexp-in-string "\n\\'" "" (buffer-string))))

(defun day-of-week (&optional date)
  "Returns the day of the week for DATE.
If DATE is nil, check today instead.

Requires the utility date to be installed."
  (with-temp-buffer
    (let ((dateProc (if (system-is-mac) "gdate" "date")))
      (if date
          (call-process dateProc nil t nil "-d" date "+%A")
        (call-process dateProc nil t nil "+%A")))
    (replace-regexp-in-string "\n\\'" "" (buffer-string))))

(defun aero/frame-recenter (&optional frame)
  "Center FRAME on the screen.

FRAME can specify a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           (display (frame-parameter frame 'display))
           (monitor-w (display-pixel-width display))
           (monitor-h (display-pixel-height display))
           ;; NS doesn't report menu bar as outside monitor
           (monitor-h (if (eq window-system 'ns) (- monitor-h 22) monitor-h))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(defun aero/ctags-create-tags (rootdir &optional ctags-cmd)
  "Generate tags database in ROOTDIR.
NOTE this requires Universal Ctags. It may work with Exuberant Ctags, but no guarantees. Definitely
does not work with GNU Ctags. If your installation of Ctags does not use the `ctags' command,
specify it with CTAGS-CMD."
  (interactive (list (read-directory-name "Root Directory: " nil nil t)))
  (let ((default-directory rootdir)
        (cmd (or ctags-cmd "ctags"))
        (buf (get-buffer-create " *aero/ctags-create-tags*")))
    (async-shell-command
     (concat cmd " --kinds-all='*' --fields='*' --extras='*' --langmap=TCL:.tcl.rvt -R")
     buf)))

(defun aero/open-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open (pcase system-type
                 (`darwin "open")
                 ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(defun pulse-line (&rest _)
  "Briefly pulse a highlight of the line at point.
This function, when bound to certain commands like scrolling, acts as a native
alternative to the beacon package."
  (pulse-momentary-highlight-one-line (point)))

(provide 'aero-lib)

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
(use-package memo
  :straight (:host gitlab :repo "thornjad/emacs-memo" :branch "main"))
(use-package async :straight t
  :commands (async-save))
(use-package request :straight t)

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

(defmacro aero/local! (&rest body)
  `(let ((default-directory user-emacs-directory))
     ,@body))

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

(defun switch-to-new-scratch-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Scratch*")))

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

(defun aero/async-write-buffer ()
  "Asynchronously write the current buffer to disk.

Really just an async wrapper around `save-buffer'"
  (interactive)
  (let* ((buf buffer-file-name)
         (fun (lambda ()
                (message (or buf "wtf"))
                (funcall #'basic-save-buffer))))

    (async-start fun)))

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

(declare-function tramp-cleanup-all-connections "tramp.el")
(defun aero/tramp-buffer-p (buffer)
  (let ((name (buffer-name buffer)))
    (string-match "^\\*tramp" name)))
(defun aero/kill-tramp ()
  "Kill and cleanup all Tramp connections. Useful for stale connections."
  (interactive)
  (cl-loop for buffer being the buffers
           do (and (aero/tramp-buffer-p buffer) (kill-buffer buffer)))
  (tramp-cleanup-all-connections))

(defun aero/kill-tags ()
  "Kill the currently-loaded TAGS file."
  (interactive)
  (when (get-buffer "TAGS")
    (kill-buffer "TAGS")))

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


;;; patches

(with-eval-after-load 'el-patch
  (with-eval-after-load 'url-http
    (el-patch-feature url-http)
    (el-patch-defun url-http-parse-headers ()
      "Parse and handle HTTP specific headers.
Return t if and only if the current buffer is still active and
should be shown to the user."
      ;; The comments after each status code handled are taken from RFC
      ;; 2616 (HTTP/1.1)
      (url-http-mark-connection-as-free (url-host url-current-object)
                                        (url-port url-current-object)
                                        url-http-process)
      ;; Pass the https certificate on to the caller.
      (when (gnutls-available-p)
        (let ((status (gnutls-peer-status url-http-process)))
          (when (or status
                    (plist-get (car url-callback-arguments) :peer))
            (setcar url-callback-arguments
                    (plist-put (car url-callback-arguments)
                               :peer status)))))
      (if (or (not (boundp 'url-http-end-of-headers))
              (not url-http-end-of-headers))
          (error "Trying to parse headers in odd buffer: %s" (buffer-name)))
      (goto-char (point-min))
      (url-http-debug "url-http-parse-headers called in (%s)" (buffer-name))
      (url-http-parse-response)
      (mail-narrow-to-head)
      ;;(narrow-to-region (point-min) url-http-end-of-headers)
      (let ((connection (mail-fetch-field "Connection")))
        ;; In HTTP 1.0, keep the connection only if there is a
        ;; "Connection: keep-alive" header.
        ;; In HTTP 1.1 (and greater), keep the connection unless there is a
        ;; "Connection: close" header
        (cond
         ((string= url-http-response-version "1.0")
          (unless (and connection
                       (string= (downcase connection) "keep-alive"))
            (delete-process url-http-process)))
         (t
          (when (and connection
                     (string= (downcase connection) "close"))
            (delete-process url-http-process)))))
      (let* ((buffer (current-buffer))
             (class (/ url-http-response-status 100))
             (success nil)
             ;; other status symbols: jewelry and luxury cars
             (status-symbol (cadr (assq url-http-response-status url-http-codes))))
        (url-http-debug "Parsed HTTP headers: class=%d status=%d"
                        class url-http-response-status)
        (when (url-use-cookies url-http-target-url)
          (url-http-handle-cookies))

        (pcase class
          ;; Classes of response codes
          ;;
          ;; 5xx = Server Error
          ;; 4xx = Client Error
          ;; 3xx = Redirection
          ;; 2xx = Successful
          ;; 1xx = Informational
          (1        ; Information messages
           ;; 100 = Continue with request
           ;; 101 = Switching protocols
           ;; 102 = Processing (Added by DAV)
           (url-mark-buffer-as-dead buffer)
           (error "HTTP responses in class 1xx not supported (%d)"
                  url-http-response-status))
          (2        ; Success
           ;; 200 Ok
           ;; 201 Created
           ;; 202 Accepted
           ;; 203 Non-authoritative information
           ;; 204 No content
           ;; 205 Reset content
           ;; 206 Partial content
           ;; 207 Multi-status (Added by DAV)
           (pcase status-symbol
             ((or 'no-content 'reset-content)
              ;; No new data, just stay at the same document
              (url-mark-buffer-as-dead buffer))
             (_
              ;; Generic success for all others.  Store in the cache, and
              ;; mark it as successful.
              (widen)
              (if (and url-automatic-caching (equal url-http-method "GET"))
                  (url-store-in-cache buffer))))
           (setq success t))
          (3        ; Redirection
           ;; 300 Multiple choices
           ;; 301 Moved permanently
           ;; 302 Found
           ;; 303 See other
           ;; 304 Not modified
           ;; 305 Use proxy
           ;; 307 Temporary redirect
           (let ((redirect-uri (or (mail-fetch-field "Location")
                                   (mail-fetch-field "URI"))))
             (pcase status-symbol
               ('multiple-choices     ; 300
                ;; Quoth the spec (section 10.3.1)
                ;; -------------------------------
                ;; The requested resource corresponds to any one of a set of
                ;; representations, each with its own specific location and
                ;; agent-driven negotiation information is being provided so
                ;; that the user can select a preferred representation and
                ;; redirect its request to that location.
                ;; [...]
                ;; If the server has a preferred choice of representation, it
                ;; SHOULD include the specific URI for that representation in
                ;; the Location field; user agents MAY use the Location field
                ;; value for automatic redirection.
                ;; -------------------------------
                ;; We do not support agent-driven negotiation, so we just
                ;; redirect to the preferred URI if one is provided.
                nil)
               ('found      ; 302
                ;; 302 Found was ambiguously defined in the standards, but
                ;; it's now recommended that it's treated like 303 instead
                ;; of 307, since that's what most servers expect.
                (setq url-http-method "GET"
                      url-http-data nil))
               ('see-other      ; 303
                ;; The response to the request can be found under a different
                ;; URI and SHOULD be retrieved using a GET method on that
                ;; resource.
                (setq url-http-method "GET"
                      url-http-data nil))
               ('not-modified   ; 304
                ;; The 304 response MUST NOT contain a message-body.
                (url-http-debug "Extracting document from cache... (%s)"
                                (url-cache-create-filename (url-view-url t)))
                (url-cache-extract (url-cache-create-filename (url-view-url t)))
                (setq redirect-uri nil
                      success t))
               ('use-proxy      ; 305
                ;; The requested resource MUST be accessed through the
                ;; proxy given by the Location field.  The Location field
                ;; gives the URI of the proxy.  The recipient is expected
                ;; to repeat this single request via the proxy.  305
                ;; responses MUST only be generated by origin servers.
                (error "Redirection thru a proxy server not supported: %s"
                       redirect-uri))
               (_
                ;; Treat everything like '300'
                nil))
             (when redirect-uri
               ;; Handle relative redirect URIs.
               (if (not (string-match url-nonrelative-link redirect-uri))
                   ;; Be careful to use the real target URL, otherwise we may
                   ;; compute the redirection relative to the URL of the proxy.
                   (setq redirect-uri
                         (url-expand-file-name redirect-uri url-http-target-url)))
               ;; Do not automatically include an authorization header in the
               ;; redirect.  If needed it will be regenerated by the relevant
               ;; auth scheme when the new request happens.
               (setq url-http-extra-headers
                     (cl-remove "Authorization"
                                url-http-extra-headers :key 'car :test 'equal))
               (let ((url-request-method url-http-method)
                     (url-request-data url-http-data)
                     (url-request-extra-headers url-http-extra-headers))
                 ;; Check existing number of redirects
                 (if (or (< url-max-redirections 0)
                         (and (> url-max-redirections 0)
                              (let ((events (car url-callback-arguments))
                                    (old-redirects 0))
                                (while events
                                  (if (eq (car events) :redirect)
                                      (setq old-redirects (1+ old-redirects)))
                                  (and (setq events (cdr events))
                                       (setq events (cdr events))))
                                (< old-redirects url-max-redirections))))
                     ;; url-max-redirections hasn't been reached, so go
                     ;; ahead and redirect.
                     (progn
                       ;; Remember that the request was redirected.
                       (setf (car url-callback-arguments)
                             (nconc (list :redirect redirect-uri)
                                    (car url-callback-arguments)))
                       ;; Put in the current buffer a forwarding pointer to the new
                       ;; destination buffer.
                       ;; FIXME: This is a hack to fix url-retrieve-synchronously
                       ;; without changing the API.  Instead url-retrieve should
                       ;; either simply not return the "destination" buffer, or it
                       ;; should take an optional `dest-buf' argument.
                       (set (make-local-variable 'url-redirect-buffer)
                            (url-retrieve-internal
                             redirect-uri url-callback-function
                             url-callback-arguments
                             (url-silent url-current-object)
                             (not (url-use-cookies url-current-object))))
                       (url-mark-buffer-as-dead buffer))
                   ;; We hit url-max-redirections, so issue an error and
                   ;; stop redirecting.
                   (url-http-debug "Maximum redirections reached")
                   (setf (car url-callback-arguments)
                         (nconc (list :error (list 'error 'http-redirect-limit
                                                   redirect-uri))
                                (car url-callback-arguments)))
                   (setq success t))))))
          (4        ; Client error
           ;; 400 Bad Request
           ;; 401 Unauthorized
           ;; 402 Payment required
           ;; 403 Forbidden
           ;; 404 Not found
           ;; 405 Method not allowed
           ;; 406 Not acceptable
           ;; 407 Proxy authentication required
           ;; 408 Request time-out
           ;; 409 Conflict
           ;; 410 Gone
           ;; 411 Length required
           ;; 412 Precondition failed
           ;; 413 Request entity too large
           ;; 414 Request-URI too large
           ;; 415 Unsupported media type
           ;; 416 Requested range not satisfiable
           ;; 417 Expectation failed
           ;; 422 Unprocessable Entity (Added by DAV)
           ;; 423 Locked
           ;; 424 Failed Dependency
           (setq success
                 (pcase status-symbol
                   ('unauthorized     ; 401
                    ;; The request requires user authentication.  The response
                    ;; MUST include a WWW-Authenticate header field containing a
                    ;; challenge applicable to the requested resource.  The
                    ;; client MAY repeat the request with a suitable
                    ;; Authorization header field.
                    (url-http-handle-authentication nil))
                   ('payment-required              ; 402
                    ;; This code is reserved for future use
                    (el-patch-remove (url-mark-buffer-as-dead buffer))
                    (el-patch-swap (error "Somebody wants you to give them money") t))
                   ('forbidden      ; 403
                    ;; The server understood the request, but is refusing to
                    ;; fulfill it.  Authorization will not help and the request
                    ;; SHOULD NOT be repeated.
                    t)
                   ('not-found      ; 404
                    ;; Not found
                    t)
                   ('method-not-allowed   ; 405
                    ;; The method specified in the Request-Line is not allowed
                    ;; for the resource identified by the Request-URI.  The
                    ;; response MUST include an Allow header containing a list of
                    ;; valid methods for the requested resource.
                    t)
                   ('not-acceptable   ; 406
                    ;; The resource identified by the request is only capable of
                    ;; generating response entities which have content
                    ;; characteristics not acceptable according to the accept
                    ;; headers sent in the request.
                    t)
                   ('proxy-authentication-required ; 407
                    ;; This code is similar to 401 (Unauthorized), but indicates
                    ;; that the client must first authenticate itself with the
                    ;; proxy.  The proxy MUST return a Proxy-Authenticate header
                    ;; field containing a challenge applicable to the proxy for
                    ;; the requested resource.
                    (url-http-handle-authentication t))
                   ('request-timeout    ; 408
                    ;; The client did not produce a request within the time that
                    ;; the server was prepared to wait.  The client MAY repeat
                    ;; the request without modifications at any later time.
                    t)
                   ('conflict     ; 409
                    ;; The request could not be completed due to a conflict with
                    ;; the current state of the resource.  This code is only
                    ;; allowed in situations where it is expected that the user
                    ;; might be able to resolve the conflict and resubmit the
                    ;; request.  The response body SHOULD include enough
                    ;; information for the user to recognize the source of the
                    ;; conflict.
                    t)
                   ('gone                          ; 410
                    ;; The requested resource is no longer available at the
                    ;; server and no forwarding address is known.
                    t)
                   ('length-required    ; 411
                    ;; The server refuses to accept the request without a defined
                    ;; Content-Length.  The client MAY repeat the request if it
                    ;; adds a valid Content-Length header field containing the
                    ;; length of the message-body in the request message.
                    ;;
                    ;; NOTE - this will never happen because
                    ;; `url-http-create-request' automatically calculates the
                    ;; content-length.
                    t)
                   ('precondition-failed    ; 412
                    ;; The precondition given in one or more of the
                    ;; request-header fields evaluated to false when it was
                    ;; tested on the server.
                    t)
                   ((or 'request-entity-too-large 'request-uri-too-large) ; 413 414
                    ;; The server is refusing to process a request because the
                    ;; request entity|URI is larger than the server is willing or
                    ;; able to process.
                    t)
                   ('unsupported-media-type ; 415
                    ;; The server is refusing to service the request because the
                    ;; entity of the request is in a format not supported by the
                    ;; requested resource for the requested method.
                    t)
                   ('requested-range-not-satisfiable ; 416
                    ;; A server SHOULD return a response with this status code if
                    ;; a request included a Range request-header field, and none
                    ;; of the range-specifier values in this field overlap the
                    ;; current extent of the selected resource, and the request
                    ;; did not include an If-Range request-header field.
                    t)
                   ('expectation-failed   ; 417
                    ;; The expectation given in an Expect request-header field
                    ;; could not be met by this server, or, if the server is a
                    ;; proxy, the server has unambiguous evidence that the
                    ;; request could not be met by the next-hop server.
                    t)
                   (_
                    ;; The request could not be understood by the server due to
                    ;; malformed syntax.  The client SHOULD NOT repeat the
                    ;; request without modifications.
                    t)))
           ;; Tell the callback that an error occurred, and what the
           ;; status code was.
           (when success
             (setf (car url-callback-arguments)
                   (nconc (list :error (list 'error 'http url-http-response-status))
                          (car url-callback-arguments)))))
          (5
           ;; 500 Internal server error
           ;; 501 Not implemented
           ;; 502 Bad gateway
           ;; 503 Service unavailable
           ;; 504 Gateway time-out
           ;; 505 HTTP version not supported
           ;; 507 Insufficient storage
           (setq success t)
           (pcase url-http-response-status
             ('not-implemented    ; 501
              ;; The server does not support the functionality required to
              ;; fulfill the request.
              nil)
             ('bad-gateway      ; 502
              ;; The server, while acting as a gateway or proxy, received
              ;; an invalid response from the upstream server it accessed
              ;; in attempting to fulfill the request.
              nil)
             ('service-unavailable    ; 503
              ;; The server is currently unable to handle the request due
              ;; to a temporary overloading or maintenance of the server.
              ;; The implication is that this is a temporary condition
              ;; which will be alleviated after some delay.  If known, the
              ;; length of the delay MAY be indicated in a Retry-After
              ;; header.  If no Retry-After is given, the client SHOULD
              ;; handle the response as it would for a 500 response.
              nil)
             ('gateway-timeout    ; 504
              ;; The server, while acting as a gateway or proxy, did not
              ;; receive a timely response from the upstream server
              ;; specified by the URI (e.g. HTTP, FTP, LDAP) or some other
              ;; auxiliary server (e.g. DNS) it needed to access in
              ;; attempting to complete the request.
              nil)
             ('http-version-not-supported ; 505
              ;; The server does not support, or refuses to support, the
              ;; HTTP protocol version that was used in the request
              ;; message.
              nil)
             ('insufficient-storage   ; 507 (DAV)
              ;; The method could not be performed on the resource
              ;; because the server is unable to store the representation
              ;; needed to successfully complete the request.  This
              ;; condition is considered to be temporary.  If the request
              ;; which received this status code was the result of a user
              ;; action, the request MUST NOT be repeated until it is
              ;; requested by a separate user action.
              nil))
           ;; Tell the callback that an error occurred, and what the
           ;; status code was.
           (when success
             (setf (car url-callback-arguments)
                   (nconc (list :error (list 'error 'http url-http-response-status))
                          (car url-callback-arguments)))))
          (_
           (error "Unknown class of HTTP response code: %d (%d)"
                  class url-http-response-status)))
        (if (not success)
            (url-mark-buffer-as-dead buffer)
          ;; Narrow the buffer for url-handle-content-transfer-encoding to
          ;; find only the headers relevant to this transaction.
          (and (not (buffer-narrowed-p))
               (mail-narrow-to-head))
          (url-handle-content-transfer-encoding))
        (url-http-debug "Finished parsing HTTP headers: %S" success)
        (widen)
        (goto-char (point-min))
        success))))


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

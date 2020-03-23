;;; aero-restart.el --- Restart Aero from within Aero -*- lexical-binding: t -*-
;;
;; Copyright (C) 2020 Jade Michael Thornton
;;
;; Based on restart-emacs.el by Iqbal Ansari, simplified based on assumptions
;; made for Aero. This includes a deliberate total lack of support for Windows.
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

(require 'aero-lib)
(require 'server)
(require 'desktop)

(defun aero/emacs-can-restart-p ()
  "Return boolean whether we can restart Emacs."
  (and
   (not (version< (emacs-version) "24.4"))
   (or (not (daemonp))
       (and
        (delq nil (mapcar (lambda (frame)
                            (frame-parameter frame 'tty))
                          (frame-list)))
        (yes-or-no-p
         "Aero Emacs daemon has tty frames, Aero cannot restore them, continue anyway? ")))))

(defun aero/get-emacs-binary ()
  "Get absolute path to binary of currently running Emacs."
  (expand-file-name invocation-name invocation-directory))

(defun aero/start-gui-using-sh ()
  "Start GUI version of Emacs using sh."
  (call-process "sh" nil
                0 nil
                "-c" (format "%s &"
                             (shell-quote-argument (aero/get-emacs-binary)))))

(defun aero/start-emacs-in-terminal ()
  "Start Emacs in current terminal."
  (suspend-emacs (format "fg ; %s -nw"
                         (shell-quote-argument (aero/get-emacs-binary)))))

(defun aero/start-daemon-using-sh ()
  "Restart Emacs daemon.
This function makes sure the new Emacs instance uses the same server-name as the
current instance"
  (call-process "sh" nil
                0 nil
                "-c" (format "%s --daemon=%s &"
                             (shell-quote-argument (aero/get-emacs-binary))
                             server-name)))

(defun aero/launch-other-emacs ()
  "Launch another Emacs session according to current platform.
Returns the function symbol without calling the function, since it will be
called by `kill-emacs-hook'."
  (cond
   ((daemonp) #'aero/start-daemon-using-sh)
   ((display-graphic-p) #'aero/start-gui-using-sh)
   (t #'aero/start-emacs-in-terminal)))

(defun aero/restart ()
  "Restart Emacs."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to restart Aero? ")
    (if (aero/emacs-can-restart-p)
        ;; We need the new emacs to be spawned after all kill-emacs-hooks have been
        ;; processed and there is nothing interesting left
        (let ((default-directory (aero/guess-startup-directory))
              (kill-emacs-hook
               (append kill-emacs-hook
                       (list (apply-partially #'aero/launch-other-emacs)))))
          (save-buffers-kill-emacs))
      (aero/log-warning "Aero is unable to restart automatically."))))

(aero-leader-def
  "ER" 'aero/restart)

(provide 'aero-restart)
;; aero-restart.el ends here

;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs
(require 'url)
(require 'xterm-color)

(defgroup wttrin nil
	"Emacs frontend for weather web service wttr.in."
	:prefix "wttrin-"
	:group 'comm)

(defcustom wttrin-cities '("Houston" "Minneapolis" "New York")
	"Cities given in quick-completion"
	:group 'wttrin
	:type 'list)

(defun wttrin-fetch-raw-string (query)
	"Get the weather information based on your QUERY."
	(let ((url-request-extra-headers '(("User-Agent" . "curl"))))
		(add-to-list 'url-request-extra-headers '("Accept-Language" . "en-US,en"))
		(with-current-buffer
				(url-retrieve-synchronously
				(concat "http://wttr.in/" query)
				(lambda (status) (switch-to-buffer (current-buffer))))
			(decode-coding-string (buffer-string) 'utf-8))))

(defun wttrin-exit ()
	(interactive)
	(quit-window t))

(defun wttrin-query (city-name)
	"Query weather of CITY-NAME via wttrin, and display the result in new buffer."
	(let ((raw-string (wttrin-fetch-raw-string city-name)))
		(if (string-match "ERROR" raw-string)
				(message "Cannot get weather data. Maybe you inputed a wrong city name?")
			(let ((buffer (get-buffer-create (format "*wttr.in - %s*" city-name))))
				(switch-to-buffer buffer)
				(setq buffer-read-only nil)
				(erase-buffer)
				(insert (xterm-color-filter raw-string))
				(goto-char (point-min))
				(re-search-forward "^$")
				(delete-region (point-min) (1+ (point)))
				(use-local-map (make-sparse-keymap))
				(local-set-key "q" 'wttrin-exit)
				(local-set-key "g" 'wttrin)
				(setq buffer-read-only t)))))

;;;###autoload
(defun wttrin (city)
	"Display weather information for CITY."
	(interactive
	(list
		(completing-read "City name: " wttrin-cities nil nil
										(when (= (length wttrin-cities) 1)
											(car wttrin-cities)))))
	(wttrin-query city))

(provide 'aero-wttrin)

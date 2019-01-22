;; based on wttrin.el by Carl X Su 0.2.0
(require 'url)
(require 'xterm-color)

(defgroup wttrin nil
	"Emacs frontend for weather web service wttr.in."
	:prefix "wttrin-"
	:group 'comm)

(defcustom wttrin-default-cities '("Houston" "Minneapolis" "New York")
	"Specify default cities list for quick completion."
	:group 'wttrin
	:type 'list)

(defcustom wttrin-default-accept-language '("Accept-Language" . "en-US,en")
	"Specify default HTTP request Header for Accept-Language."
	:group 'wttrin
	:type '(list)
	)

(defun wttrin-fetch-raw-string (query)
	"Get the weather information based on your QUERY."
	(let ((url-request-extra-headers '(("User-Agent" . "curl"))))
		(add-to-list 'url-request-extra-headers wttrin-default-accept-language)
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
		(completing-read "City name: " wttrin-default-cities nil nil
										(when (= (length wttrin-default-cities) 1)
											(car wttrin-default-cities)))))
	(wttrin-query city))

(provide 'wttrin)

;; END wttrin

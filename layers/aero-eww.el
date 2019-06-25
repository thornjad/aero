;; -*- lexical-binding: t -*-
;;
;; Copyright (c) 2018-2019 Jade Michael Thornton
;;
;; This program is free software; you may redistribute it and/or modify it under
;; the terms of the GNU General Public License version 3, as published by the
;; Free Software Foundation. This program carries no warranty whatsoever,
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose. See </license> for more details.
;;
;; This file is not part of GNU Emacs

(use-package eww
	:config
	(setq eww-search-prefix "https://duckduckgo.com/lite?q="
        browse-url-browser-function 'eww-browse-url)
	(defun add-title-to-eww-buffer-name ()
		"Rename eww mode buffer so the title of the page is displayed, making
		 fake-tabbed-browsing easier"
		(let ((title (plist-get eww-data :title)))
			(when (eq major-mode 'eww-mode)
				(if title
						(rename-buffer (concat "eww - " title) t)
					(rename-buffer "eww" t)))))
	(add-hook 'eww-after-render-hook 'add-title-to-eww-buffer-name))

(provide 'aero-eww)

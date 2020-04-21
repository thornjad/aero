;; -*- lexical-binding: t -*-
;;
;; Aero core prelude layer
;;
;; Copyright (c) 2020 Jade Michael Thornton
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

(use-package docker :straight t
  :commands docker
  :bind ("C-c d" . docker)
  :init
  (use-package docker-image     :commands docker-images)
  (use-package docker-container :commands docker-containers)
  (use-package docker-volume    :commands docker-volumes)
  (use-package docker-network   :commands docker-containers)
  (use-package docker-machine   :commands docker-machines)
  (use-package docker-compose   :commands docker-compose))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")

(use-package docker-tramp
  :after tramp
  :defer 5)

(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(provide 'aero-docker)

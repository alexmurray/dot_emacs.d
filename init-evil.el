;;; init-evil.el --- Initialise evil and related packages

;;; Commentary:
;;


;;; Code:
(require 'evil)
(evil-mode t)

;; enable surround
(require 'surround)
(global-surround-mode 1)

(provide 'init-evil)

;;; init-evil.el ends here

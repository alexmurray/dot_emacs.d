;;; init-smart-mode-line.el --- Initialise the smart-mode-line package

;;; Commentary:
;;

;;; Code:
(require 'smart-mode-line)

(add-hook 'after-init-hook 'sml/setup)

(provide 'init-smart-mode-line)

;;; init-smart-mode-line.el ends here

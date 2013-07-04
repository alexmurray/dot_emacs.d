;;; init-company-mode.el --- Initialise the company package

;;; Commentary:
;;

;;; Code:
(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "diminish"
  '(diminish 'company-mode " C"))

(provide 'init-company-mode)

;;; init-company-mode.el ends here

;;; init-company.el --- Initialise the company package
;; enable company-mode in all buffers

;;; Commentary:
;;

;;; Code:
(require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "diminish"
  '(diminish 'company-mode))

(provide 'init-company)

;;; init-company.el ends here

;;; init-company.el --- Initialise the company package
;; enable company-mode in all buffers

;;; Commentary:
;;

;;; Code:
(require 'company)

;; some better default values
(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)

;; put most often used completions at stop of list
(setq company-transformers '(company-sort-by-occurrence))

;; enable company in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'diminish
  '(diminish 'company-mode))

;; add company-auctex
(require 'company-auctex)
(company-auctex-init)

(provide 'init-company)

;;; init-company.el ends here

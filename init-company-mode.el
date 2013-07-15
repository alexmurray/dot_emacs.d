;;; init-company-mode.el --- Initialise the company package

;;; Commentary:
;;

;;; Code:
(require 'company)

;; enable company-mode completion faster
(setq company-idle-delay 0.2)

(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "evil"
  ;; make evil and company-mode play nicely when doing repeated
  ;; commands in evil with .
  '(mapc #'evil-declare-change-repeat
	 '(company-complete-common
	   company-select-next
	   company-select-previous
	   company-complete-selection)))

(eval-after-load "diminish"
  '(diminish 'company-mode " C"))

(provide 'init-company-mode)

;;; init-company-mode.el ends here

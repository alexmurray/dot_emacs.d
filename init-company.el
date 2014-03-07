;;; init-company.el --- Initialise the company package
;; enable company-mode in all buffers

;;; Commentary:
;;

;;; Code:
(require 'company)

;; add some better keybindings for company
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete)

;; enable company in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "diminish"
  '(diminish 'company-mode))

(provide 'init-company)

;;; init-company.el ends here

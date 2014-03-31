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
(global-set-key (kbd "C-x y") 'company-yasnippet)

;; put most often used completions at stop of list
(setq company-transformers '(company-sort-by-occurrence))

;; enable company in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load "diminish"
  '(diminish 'company-mode))

(provide 'init-company)

;;; init-company.el ends here

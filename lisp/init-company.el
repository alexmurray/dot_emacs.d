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

;; nicer keybinding
(define-key company-active-map (kbd "<tab>") 'company-complete)

;; put most often used completions at stop of list
(setq company-transformers '(company-sort-by-occurrence))

;; enable company in all buffers
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'diminish
  '(diminish 'company-mode))

;; choose better colours for company
(set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
(set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
(set-face-attribute 'company-preview nil :background "black")
(set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
(set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
(set-face-attribute 'company-scrollbar-fg nil :background "gray40")

;; add company-auctex
(require 'company-auctex)
(company-auctex-init)

(provide 'init-company)

;;; init-company.el ends here

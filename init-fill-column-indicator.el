;;; init-fill-column-indicator.el --- Initialise fill-column-indicator

;;; Commentary:
;; 

;;; Code:
(require 'fill-column-indicator)
;; use fill-column-indicator in all buffers
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

(provide 'init-fill-column-indicator)

;;; init-fill-column-indicator.el ends here

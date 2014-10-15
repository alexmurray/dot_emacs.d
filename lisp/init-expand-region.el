;;; init-expand-region.el --- Initialise the expand-region package
;; expand-region

;;; Commentary:
;; 

;;; Code:

(require 'bind-key)
(bind-key "C-=" 'er/expand-region)

(provide 'init-expand-region)

;;; init-expand-region.el ends here

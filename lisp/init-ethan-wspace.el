;;; init-ethan-wspace.el --- Initialise the ethan-wspace package

;;; Commentary:
;; 

;;; Code:
(require 'ethan-wspace)

(global-ethan-wspace-mode 1)

(eval-after-load "diminish"
  '(diminish 'ethan-wspace-mode))

(provide 'init-ethan-wspace)

;;; init-ethan-wspace.el ends here

;;; init-ethan-wspace.el --- Initialise the ethan-wspace package

;;; Commentary:
;; 

;;; Code:
(require 'ethan-wspace)

;; ethan-wspace-mode raises lots of warnings if this is enabled...
;; hopefully this doesn't cause problems
(setq mode-require-final-newline nil)

(global-ethan-wspace-mode 1)

;; disable ethan-wspace caring about tabs in Makefile's
(defun makefile-tabs-are-less-evil ()
  "Disable ethan-wspace from caring about tabs in Makefile's."
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(add-hook 'makefile-mode-hook 'makefile-tabs-are-less-evil)
  
(eval-after-load 'diminish
  '(diminish 'ethan-wspace-mode))

(provide 'init-ethan-wspace)

;;; init-ethan-wspace.el ends here

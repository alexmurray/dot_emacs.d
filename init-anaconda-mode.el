;;; init-anaconda-mode.el --- Initialise the anaconda package

;;; Commentary:
;;

;;; Code:
(require 'anaconda-mode)

(defun anaconda-mode-setup ()
  ;; turn on anaconda-mode
  (anaconda-mode t)
  ;; integrate into eldoc
  (anaconda-eldoc))

;; use anaconda-mode for python
(add-hook 'python-mode-hook 'anaconda-mode-setup)

;; enable anaconda-mode backend for company
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(provide 'init-anaconda-mode)

;;; init-anaconda-mode.el ends here

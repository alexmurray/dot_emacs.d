;;; init-anaconda-mode.el --- Initialise the anaconda package

;;; Commentary:
;;

;;; Code:
(require 'anaconda-mode)

(defun anaconda-mode-setup ()
  "Setup anaconda-mode for python programming."
  ;; turn on anaconda-mode
  (anaconda-mode t)
  ;; integrate into eldoc
  (anaconda-eldoc)
  (eval-after-load 'company
    '(setq-local company-backends '((company-anaconda company-dabbrev-code company-yasnippet) company-gtags))))

;; use anaconda-mode for python
(add-hook 'python-mode-hook 'anaconda-mode-setup)

(provide 'init-anaconda-mode)

;;; init-anaconda-mode.el ends here

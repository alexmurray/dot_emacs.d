;;; init-flycheck.el --- Initialise flycheck package

;;; Commentary:
;;

(require 'flycheck)

;;; Code:

;; use flycheck for syntax checking in all buffers
(flycheck-mode t)
(global-flycheck-mode +1)

;; use flycheck-pos-tip to display errors
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(require 'flycheck-color-mode-line)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(eval-after-load 'diminish
  '(diminish 'flycheck-mode "FyC"))

(provide 'init-flycheck)

;;; init-flycheck.el ends here

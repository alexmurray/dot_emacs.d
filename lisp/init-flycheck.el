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

;; diminish to check icon from fontawesome
(eval-after-load 'diminish
  '(diminish 'flycheck-mode (concat " " [#xF00C])))

(provide 'init-flycheck)

;;; init-flycheck.el ends here

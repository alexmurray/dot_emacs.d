;;; init-column-enforce-mode.el --- Initialise column-enforce-mode package

;;; Commentary:
;;

;;; Code:
(require 'column-enforce-mode)

(column-enforce-mode t)

(eval-after-load 'diminish
  '(diminish 'column-enforce-mode))

(provide 'init-column-enforce-mode)

;;; init-column-enforce-mode.el ends here

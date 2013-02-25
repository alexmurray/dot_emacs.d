;;; init-change-inner.el --- Initialise the change-inner package

;;; Commentary:
;;

;;; Code:

(require 'change-inner)

(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(provide 'init-change-inner)

;;; init-change-inner.el ends here

;;; init-flycheck-cohda-c-style.el --- Initialise the flycheck-cohda-c-style package

;;; Commentary:
;;

;;; Code:

(add-to-list 'load-path (expand-file-name "vendor/flycheck-cohda-c-style" user-emacs-directory))

;; don't absolutely require
(unless (require 'flycheck-cohda-c-style nil t)
  (apm-notify "flycheck-cohda-c-style not found"))

(provide 'init-flycheck-cohda-c-style)

;;; init-flycheck-cohda-c-style.el ends here

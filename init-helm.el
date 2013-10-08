;;; init-helm.el --- Initialise the helm package

;;; Commentary:
;;

;;; Code:
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-mini)

(provide 'init-helm)

;;; init-helm.el ends here

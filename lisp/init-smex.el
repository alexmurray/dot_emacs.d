;;; init-smex.el --- Initialise smex
;; smex

;;; Commentary:
;;

;;; Code:

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(require 'bind-key)
(bind-key "M-x" 'smex)

(provide 'init-smex)

;;; init-smex.el ends here

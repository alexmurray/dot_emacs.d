;;; init-smex.el --- Initialise smex
;; smex

;;; Commentary:
;; 

;;; Code:

(setq smex-save-file (expand-file-name (concat user-emacs-directory ".smex-items")))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(provide 'init-smex)

;;; init-smex.el ends here

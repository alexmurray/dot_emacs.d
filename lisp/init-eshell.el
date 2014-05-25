;;; init-eshell.el --- Initialise eshell-mode

;;; Commentary:
;;

;;; Code:

(defun setup-eshell-mode ()
  "Initialise 'eshell-mode'."
  (setq mode-name (concat "e" [#xF120])))

(add-hook 'eshell-mode-hook 'setup-eshell-mode)

(provide 'init-eshell)

;;; init-eshell.el ends here

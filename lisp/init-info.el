;;; init-info.el --- Initialise info

;;; Commentary:
;;

;;; Code:

(defun info-mode-setup ()
  "Setup Info mode."
  (setq mode-name (concat "" [#xF129])))

(add-hook 'info-mode-hook 'info-mode-setup)

(provide 'init-info)

;;; init-info.el ends here

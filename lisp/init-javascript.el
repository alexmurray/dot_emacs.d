;;; init-javascript.el --- Initialise javascript

;;; Commentary:
;;

;;; Code:

(defun js2-mode-setup ()
  "Setup js2-mode."
  (setq mode-name "js2"))

(add-hook 'js2-mode-hook 'js2-mode-setup)

(provide 'init-javascript)

;;; init-javascript.el ends here

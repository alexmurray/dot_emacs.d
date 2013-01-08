;;; init-javascript.el --- Initialise javascript

;;; Commentary:
;;

;;; Code:

;; js2-mode doesn't inherit from prog-mode so make sure we get our own
;; custom goodness
(add-hook 'js2-mode-hook 'common-programming-setup)

(provide 'init-javascript)

;;; init-javascript.el ends here

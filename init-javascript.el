;;; init-javascript.el --- Initialise javascript

;;; Commentary:
;;

;;; Code:

(eval-after-load 'js
  ;; On-the-fly syntax checking
  '(add-hook 'js-mode-hook 'flycheck-mode-on))

(provide 'init-javascript)

;;; init-javascript.el ends here

;;; init-rainbow-mode.el --- Initialise rainbow-mode package

;;; Commentary:
;;

;;; Code:

;; rainbow mode - for colouring strings that represent colors
;; enable rainbow mode automatically for css and html modes
(dolist (hook '(css-mode-hook html-mode-hook))
  (add-hook hook 'rainbow-mode))

(provide 'init-rainbow-mode)

;;; init-rainbow-mode.el ends here

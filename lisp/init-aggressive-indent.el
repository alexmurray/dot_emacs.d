;;; init-aggressive-indent.el --- Initialise the aggressive-indent package

;;; Commentary:
;;

(require 'aggressive-indent)

;;; Code:

;; enable on all buffers
(global-aggressive-indent-mode 1)

;; diminish to nothing
(eval-after-load 'diminish
  '(diminish 'aggressive-indent-mode))

(provide 'init-aggressive-indent)

;;; init-aggressive-indent.el ends here

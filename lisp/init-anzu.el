;;; init-anzu.el --- Initialize anzu package to show matches / position


;;; Commentary:
;;

;;; Code:

(require 'anzu)

(global-anzu-mode)

(eval-after-load 'diminish
  '(diminish 'anzu-mode))

(provide 'init-anzu)

;;; init-anzu.el ends here

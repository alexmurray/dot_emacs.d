;;; init-gtags.el --- Initiali se gtags

;;; Commentary:
;;

;;; Code:
(require 'gtags)

(eval-after-load 'gtags
  '(define-key gtags-mode-map [mouse-2] 'mouse-yank-primary))

(provide 'init-gtags)

;;; init-gtags.el ends here

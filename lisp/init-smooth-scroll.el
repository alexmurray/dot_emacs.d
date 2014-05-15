;;; init-smooth-scroll.el --- Initialise the smooth-scroll package

;;; Commentary:
;;

;;; Code:
(require 'smooth-scroll)

(smooth-scroll-mode)

(eval-after-load 'diminish
  '(diminish 'smooth-scroll-mode))

(provide 'init-smooth-scroll)

;;; init-smooth-scroll.el ends here

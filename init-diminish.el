;;; init-diminish.el --- Initialise diminish
;; diminish

;;; Commentary:
;;

(require 'diminish)

;;; Code:
(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))
(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))

(provide 'init-diminish)

;;; init-diminish.el ends here

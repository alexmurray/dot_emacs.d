;;; init-git-gutter.el --- Initialise the git-gutter package

;;; Commentary:
;;

;;; Code:
(require 'git-gutter)

(global-git-gutter-mode t)

(eval-after-load "diminish"
  '(diminish 'git-gutter-mode))

(provide 'init-git-gutter)

;;; init-git-gutter.el ends here

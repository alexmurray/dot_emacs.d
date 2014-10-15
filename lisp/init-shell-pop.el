;;; init-shell-pop.el --- Initialise the shell-pop package

;;; Commentary:
;; 

(require 'shell-pop)

;;; Code:

(require 'bind-key)
(bind-key "C-c ;" 'shell-pop)

(provide 'init-shell-pop)

;;; init-shell-pop.el ends here

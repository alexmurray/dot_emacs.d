;;; init-butler.el --- Initialise butler package for Jenkins integration

;;; Commentary:
;;

;;; Code:
(require 'butler)

;; required due to bug in butler.el -
;; https://github.com/AshtonKem/Butler/issues/6#issuecomment-25698843
(load-library "org-exp")

(add-to-list 'butler-server-list
             '(jenkins "cw-dev03"
                       (server-address . "http://cw-dev03.cohda.wireless:8080")
                       (auth-file . "~/.butler-authinfo.gpg")))

(provide 'init-butler)

;;; init-butler.el ends here

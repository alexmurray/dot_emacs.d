;;; init-dsvn.el --- Initialise dsvn
;;

;;; Commentary:
;;

;;; Code:
(require 'dsvn)
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)

(require 'vc-svn)

(provide 'init-dsvn)

;;; init-dsvn.el ends here

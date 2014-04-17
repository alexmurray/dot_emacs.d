;;; init-trackwiki-mode.el --- Initialise trac-wiki package

;;; Commentary:
;;


;;; Code:
(require 'trac-wiki)

(setq trac-projects '(("mk2"
                       :endpoint "http://projects.cohda.wireless:8000/trac/mk2/login/xmlrpc"
                       :login "amurray")))

(provide 'init-trac-wiki)

;;; init-trac-wiki.el ends here

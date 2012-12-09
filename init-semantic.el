;;; init-semantic.el --- Initialise semantic

;;; Commentary:
;;

(require 'semantic)
(require 'semantic/db)
;;; Code:

;; semantic and semanticdb - stores semantic information in a db so is
;; faster to compute next time a file is loaded
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
;; show summary of tag at point when idle
(global-semantic-idle-summary-mode 1)

(provide 'init-semantic)

;;; init-semantic.el ends here

;;; init-semantic.el --- Initialise semantic

;;; Commentary:
;;

(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(require 'semantic/db)
;;; Code:

;; semantic and semanticdb - stores semantic information in a db so is
;; faster to compute next time a file is loaded
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
;; show summary of tag at point when idle
(global-semantic-idle-summary-mode 1)

;; enable support for gnu global - to generate a global database for a
;; project, something like the following works:
;; find . -not -iwholename "/.svn" -name *.[ch] | gtags -f -
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(provide 'init-semantic)

;;; init-semantic.el ends here

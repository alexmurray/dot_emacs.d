;;; init-semantic.el --- Initialise semantic

;;; Commentary:
;;

(require 'semantic)
(require 'semantic/ia)
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/db)
;;; Code:

;; semantic and semanticdb - stores semantic information in a db so is
;; faster to compute next time a file is loaded
(semantic-mode 1)
(global-semanticdb-minor-mode 1)

;; show summary of tag at point when idle
(global-semantic-idle-summary-mode 1)

;; get semantic to index back to various top-level marker files
(defvar apm-semantic-project-root-markers
 '(".git" "GTAGS" "TAGS" ".svn" "Makefile"))

(setq semanticdb-project-root-functions
      (mapcar #'(lambda (file)
		  (eval `(lambda (directory)
			   (locate-dominating-file directory ,file))))
	      apm-semantic-project-root-markers))

;; parse include headers in idle time
(setq semantic-idle-work-update-headers-flag t)

;; setup for c, c++ and java
(dolist (mode '(c-mode c++-mode java-mode))
  ;; enable support for gnu global - to generate a global database for a
  ;; project, something like the following works:
  ;; find . -not -iwholename "/.svn" -name *.[ch] | gtags -f -
  (semanticdb-enable-gnu-global-databases mode))

(provide 'init-semantic)

;;; init-semantic.el ends here

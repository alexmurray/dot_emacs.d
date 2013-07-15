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

;; get semantic to index back to various top-level marker files
(defvar apm-semantic-project-root-markers
 '(".git" "cohda-version" "GTAGS" "TAGS" "Makefile"))

(setq semanticdb-project-root-functions
      (mapcar #'(lambda (file)
		  (eval `(lambda (directory) (locate-dominating-file directory ,file))))
	      apm-semantic-project-root-markers))

;; setup for c, c++ and java
(defvar apm-semantic-modes
  '(c-mode c++-mode java-mode))

(dolist (mode apm-semantic-modes)
  (setq-mode-local mode
		   semanticdb-find-default-throttle
		   '(project unloaded system recursive omniscience))
  ;; enable support for gnu global - to generate a global database for a
  ;; project, something like the following works:
  ;; find . -not -iwholename "/.svn" -name *.[ch] | gtags -f -
  (semanticdb-enable-gnu-global-databases mode))

(provide 'init-semantic)

;;; init-semantic.el ends here

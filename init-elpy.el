;;; init-elpy.el --- Initialise the elpy package

;;; Commentary:
;;

;;; Code:
(require 'elpy)

;; enable elpy but skip it initializing variables so it doesn't
;; override company and yasnippet bits etc
(elpy-enable t)
(elpy-use-ipython)

(provide 'init-elpy)

;;; init-elpy.el ends here

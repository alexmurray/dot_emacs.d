;;; init-java.el --- Initialise java-mode for Java coding

;;; Commentary:
;;

;;; Code:
(defun java-mode-setup ()
  (c-set-style 'ellemtel)
  (setq c-basic-offset 4))

(add-hook 'java-mode 'java-mode-setup)

(provide 'init-java)

;;; init-java.el ends here

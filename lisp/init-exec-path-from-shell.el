;;; init-exec-path-from-shell.el --- initialise exec-path-from-shell package

;;; Commentary:
;;

;;; Code:

;; use only on OSX GUI to get path from shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path-from-shell)

;;; init-exec-path-from-shell.el ends here

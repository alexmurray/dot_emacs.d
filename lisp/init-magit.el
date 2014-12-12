;;; init-magit.el --- Initialise magit
;; magit

;;; Commentary:
;; 

;;; Code:
(require 'magit)

(require 'bind-key)
(bind-key "C-x g" 'magit-status)

;; full screen magit-status
(defun apm-fullscreen-magit-status (orig-fun &rest args)
  "Create a fullscreen `magit-status' via ORIG-FUN and ARGS."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(advice-add 'magit-status :around #'apm-fullscreen-magit-status)

(defun magit-quit-session ()
  "Restore the previous window config and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(eval-after-load 'diminish
  '(diminish 'magit-auto-revert-mode))

(provide 'init-magit)

;;; init-magit.el ends here

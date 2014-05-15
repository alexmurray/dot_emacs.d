;;; init-magit.el --- Initialise magit
;; magit

;;; Commentary:
;; 

;;; Code:
(require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

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

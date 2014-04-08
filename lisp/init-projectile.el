;;; init-projectile.el --- Initialise the projectile package

;;; Commentary:
;;

;;; Code:
(require 'projectile)

(projectile-global-mode)

(require 'helm-projectile)

;; use projectile keybinding which includes helm-mini
(global-set-key (kbd "C-c h") 'helm-projectile)

(eval-after-load "diminish"
  '(diminish 'projectile-mode "Prj"))

(provide 'init-projectile)

;;; init-projectile.el ends here

;;; init-projectile.el --- Initialise the projectile package

;;; Commentary:
;;

;;; Code:
(require 'projectile)

(projectile-global-mode)

(require 'helm-projectile)

;; use projectile keybinding which includes helm-mini
(require 'bind-key)
(bind-key "C-c h" 'helm-projectile)

;; diminish to gear icon from fontawesome
(eval-after-load 'diminish
  '(diminish 'projectile-mode (concat " " [#xF013])))

(provide 'init-projectile)

;;; init-projectile.el ends here

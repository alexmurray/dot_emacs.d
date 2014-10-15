;;; init-mc.el --- Initialise multiple-cursors
;; multiple cursors

;;; Commentary:
;;

;;; Code:
(require 'multiple-cursors)
(setq mc/unsupported-minor-modes '(company-mode flyspell-mode))
(eval-after-load 'evil
  '(progn
     (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
     (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state)))

(require 'bind-key)
(bind-key "C-S-c C-S-c" 'mc/edit-lines)
(bind-key "C->" 'mc/mark-next-like-this)
(bind-key "C-<" 'mc/mark-previous-like-this)
(bind-key "C-c C-<" 'mc/mark-all-like-this)

(provide 'init-mc)

;;; init-mc.el ends here

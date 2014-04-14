;;; init-elisp-slime-nav.el --- Initialise the elisp-slime-nav package

;;; Commentary:
;;

;;; Code:
(require 'elisp-slime-nav)

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))
(provide 'init-elisp-slime-nav)

;;; init-elisp-slime-nav.el ends here

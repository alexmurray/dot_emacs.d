;;; init-programming.el --- Initialise common programming options

;; Common programming related stuff for a range of modes

;;; Commentary:
;;

;;; Code:
(defun common-programming-setup ()
  "Tweaks and customisations for all programming modes."
  ;; delete trailing whitespace by default
  (enable-delete-trailing-whitespace)
  ;; show trailing whitespace
  (setq show-trailing-whitespace t)
  ;; turn on spell checking for strings and comments
  (flyspell-prog-mode)
  ;; workaround bug in autocomplete and flyspell
  (ac-flyspell-workaround)
  ;; highlight TODO and fixme so it looks scary
  (font-lock-add-keywords nil
   '(("\\<\\(TODO\\|todo\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'common-programming-setup)

(provide 'init-programming)

;;; init-programming.el ends here

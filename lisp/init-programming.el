;;; init-programming.el --- Initialise common programming options

;; Common programming related stuff for a range of modes

;;; Commentary:
;;

;;; Code:
(defun common-programming-setup ()
  "Tweaks and customisations for all programming modes."
  ;; turn on spell checking for strings and comments
  (flyspell-prog-mode)
  ;; highlight TODO and fixme so it looks scary
  (font-lock-add-keywords nil '(("\\<\\(TODO\\|todo\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'common-programming-setup)

;; show suspicious c constructs automatically
(global-cwarn-mode 1)
;; diminish cwarn from modeline
(eval-after-load "diminish"
  '(diminish 'cwarn-mode))

(provide 'init-programming)

;;; init-programming.el ends here

;;; init-emacs-lisp.el --- Initialise emacs-lisp

;;; Commentary:
;; 

;;; Code:

(defun emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  (setq mode-name "el")
  ;; use aggressive indent
  (aggressive-indent-mode 1)
  (eldoc-mode t)
  ;; use smartparens in strict mode for lisp
  (smartparens-strict-mode +1))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-setup)

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here

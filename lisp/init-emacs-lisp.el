;;; init-emacs-lisp.el --- Initialise emacs-lisp

;;; Commentary:
;; 

;;; Code:

(defun emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  (setq mode-name "el")
  (eldoc-mode t)
  (font-lock-pretty-lambdas)
  ;; use smartparens in strict mode for lisp
  (smartparens-strict-mode +1)
  ;; enable helpers for evil to maintain parens
  (evil-paredit-mode t))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-setup)

(provide 'init-emacs-lisp)

;;; init-emacs-lisp.el ends here

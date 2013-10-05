;;; init-python.el --- Initialise python
;; Python

;;; Commentary:
;; 

;;; Code:

(defun python-mode-setup ()
  "Tweaks and customisations for `python-mode'."
  ;; setup python mode for eldoc and auto-complete with semantic
  (eldoc-mode)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'python-mode-hook 'python-mode-setup)

;; enable pymacs / ropemacs support
(when (locate-library "pymacs")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-guess-project t)
  (setq ropemacs-separate-doc-buffer t)
  (setq ropemacs-enable-autoimport nil))

(provide 'init-python)

;;; init-python.el ends here

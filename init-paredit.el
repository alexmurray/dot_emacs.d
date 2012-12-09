;;; init-paredit.el --- Initialise the paredit package
;; paredit

;;; Commentary:
;; 

;;; Code:
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(autoload 'enable-paredit-mode "paredit" "Turn on paredit mode" t)
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook hook 'font-lock-pretty-lambdas)
  (add-hook hook 'enable-paredit-mode))

(defun conditionally-enable-paredit-mode ()
  "Enable function `paredit-mode' during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Suspend mode MODE-NAME during cua-rect-selection."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load "cua-rect"
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name -1)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))
(suspend-mode-during-cua-rect-selection 'paredit-mode)

;; diminish paredit
(eval-after-load "diminish"
  '(eval-after-load "paredit"
     '(diminish 'paredit-mode " P")))

(provide 'init-paredit)

;;; init-paredit.el ends here

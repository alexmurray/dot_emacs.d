;;; init-evil.el --- Initialise evil and related packages

;;; Commentary:
;;


;;; Code:
(require 'evil)
(evil-mode t)

;; enable surround
(require 'surround)
(global-surround-mode 1)

;; enable matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

(defadvice evil-jump-to-tag (around evil-gtags-jump-to-tag activate)
  "Make use of gtags if possible instead of etags for finding definitions."
  (cond
   ((bound-and-true-p gtags-mode) (gtags-find-tag-from-here))
   (t ad-do-it)))

(dolist (mode '(comint-mode
                eshell-mode
                inferior-emacs-lisp-mode
                gtags-select-mode
                magit-branch-manager-mode
                pylookup-mode
                semantic-symref-results-mode
                shell-mode
                term-mode))
  (evil-set-initial-state mode 'emacs))

(provide 'init-evil)

;;; init-evil.el ends here

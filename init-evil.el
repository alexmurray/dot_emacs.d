;;; init-evil.el --- Initialise evil and related packages

;;; Commentary:
;;


;;; Code:
(require 'evil)
(evil-mode t)

;; ensure can more easily distinguish current evil mode
(setq evil-normal-state-tag (propertize "N" 'face '((:background "green" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "orange" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "red")))
      evil-motion-state-tag (propertize "M" 'face '((:background "blue")))
      evil-visual-state-tag (propertize "V" 'face '((:background "grey80" :foreground "black")))
      evil-operator-state-tag (propertize "O" 'face '((:background "purple"))))

(dolist (mode '(comint-mode
                eshell-mode
                inferior-emacs-lisp-mode
                gtags-select-mode
                magit-branch-manager-mode
                pylookup-mode
                semantic-symref-results-mode
                shell-mode
                svn-status-mode
                term-mode))
  (evil-set-initial-state mode 'emacs))

(defadvice evil-jump-to-tag (around evil-gtags-jump-to-tag activate)
  "Make use of gtags if possible instead of etags for finding definitions."
  (cond
   ((bound-and-true-p gtags-mode) (gtags-find-tag-from-here))
   (t ad-do-it)))

;; enable surround
(require 'surround)
(global-surround-mode 1)

;; enable matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; visualstar allows to start a */# search from visual selection
(require 'evil-visualstar)

;; evil-leader config
(setq evil-leader/leader ","
      evil-leader/in-all-states t)
(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cc" 'evilnc-comment-or-uncomment-to-the-line)

(provide 'init-evil)

;;; init-evil.el ends here

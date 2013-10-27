;;; init-evil.el --- Initialise evil and related packages

;;; Commentary:
;;


;;; Code:
(require 'evil)
(evil-mode t)

;; make cursor easier to see
(setq evil-normal-state-cursor '("#b294bb" box))
(setq evil-insert-state-cursor '("#de935f" bar))
(setq evil-emacs-state-cursor '("#cc6666" box))

;; ensure can more easily distinguish current evil mode
(setq evil-normal-state-tag (propertize "N" 'face '((:background "#b294bb" :foreground "black")))
      evil-emacs-state-tag (propertize "E" 'face '((:background "#cc6666" :foreground "black")))
      evil-insert-state-tag (propertize "I" 'face '((:background "#de935f" :foreground "black")))
      evil-motion-state-tag (propertize "M" 'face '((:background "#81a2be")))
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
  "cc" 'evilnc-comment-or-uncomment-to-the-line
  "gr" 'gtags-find-rtag
  "gy" 'gtags-find-symbol
  "cg" 'apm-gtags-create-or-update
  "ma" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  "md" 'mc/mark-all-like-this-dwim
  "rw" 'rotate-windows
  "x"  'er/expand-region)


(provide 'init-evil)

;;; init-evil.el ends here

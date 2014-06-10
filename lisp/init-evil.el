;;; init-evil.el --- Initialise evil and related packages

;;; Commentary:
;;


;;; Code:
(require 'evil)

;; make cursor easier to see
(setq evil-normal-state-cursor '("#b294bb" box))
(setq evil-insert-state-cursor '("#de935f" bar))
(setq evil-emacs-state-cursor '("#cc6666" box))

(dolist (mode '(comint-mode
                eshell-mode
                inferior-emacs-lisp-mode
                git-rebase-mode
                gtags-select-mode
                magit-branch-manager-mode
                pylookup-mode
                semantic-symref-results-mode
                shell-mode
                svn-status-mode
                term-mode))
  (evil-set-initial-state mode 'emacs))

(defadvice evil-jump-to-tag (around apm-gtags-jump-to-tag activate)
  "Make use of gtags / elisp-slime-nav if possible instead of etags for finding definitions."
  (cond
   ((bound-and-true-p gtags-mode) (gtags-find-tag-from-here))
   ((bound-and-true-p elisp-slime-nav-mode) (elisp-slime-nav-find-elisp-thing-at-point (thing-at-point 'symbol)))
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
(global-evil-leader-mode 1)

(evil-leader/set-key
  "ci" 'surround-change
  "si" 'evilmi-select-items
  "di" 'evilmi-delete-items
  "cc" 'evilnc-comment-or-uncomment-lines
  "gr" 'gtags-find-rtag
  "gy" 'gtags-find-symbol
  "cg" 'apm-gtags-create-or-update
  "ma" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  "md" 'mc/mark-all-like-this-dwim
  "rw" 'rotate-windows
  "x"  'er/expand-region)

;; evil-args
(require 'evil-args)

;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)

;; bind evil-jump-out-args
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; only start at end so *Messages* and *scratch* get evil leader set
;; etc.
(evil-mode t)

(provide 'init-evil)

;;; init-evil.el ends here

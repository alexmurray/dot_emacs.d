;;; init-slime.el --- Initlialise slime and related packages
 ;; slime
;;; Code:

(add-to-list 'load-path (expand-file-name "vendor/slime-2012-10-10" user-emacs-directory))

;;; Commentary:
;;

(require 'slime-autoloads)
(setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
;; autoload slime when you open a .lisp file
(defun slime-mode-setup ()
  "Setup slime mode to automatically start slime if not connected."
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'slime-mode-hook 'slime-mode-setup)
;; autoclose Emacs even if Lisp processes are running
(setq slime-kill-without-query-p t)
;; enable paredit for slime modes
(eval-after-load "paredit"
  (dolist (hook '(slime-mode-hook slime-repl-mode-hook))
    (add-hook hook 'enable-paredit-mode)))
;; enable repl and fancy etc
(slime-setup '(slime-fancy slime-repl slime-asdf slime-tramp))

;; slime autocomplete
(eval-after-load "auto-complete"
  '(progn
     (require 'ac-slime)
  ;; set load slime-ac on slime modes and set ac-modes to include slime
     '(dolist (mode '(slime-mode slime-repl-mode))
	(add-hook (intern (concat (symbol-name mode) "-hook")) 'set-up-slime-ac)
	(add-to-list 'ac-modes mode))))

(provide 'init-slime)

;;; init-slime.el ends here

;;; init.el --- Starting point for Alex Murray's Emacs Configuration

;;; Commentary:
;;

;;; Code:

;; custom init files live under lisp/
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-misc)
(require 'init-editing)
(require 'init-eshell)
(require 'init-gtags)
(require 'init-ido)
(require 'init-ui)
(require 'init-semantic)
(require 'init-uniquify)
;; vendor packages
(require 'init-vendor)
;; external packages from elpa / marmalade
(require 'init-ace-jump-mode)
(require 'init-ack-and-a-half)
(require 'init-anaconda-mode)
(require 'init-anzu)
(require 'init-company)
(require 'init-column-enforce-mode)
(require 'init-diff-hl)
(require 'init-diminish)
(require 'init-dsvn)
(require 'init-elisp-slime-nav)
(require 'init-exec-path-from-shell)
(require 'init-ethan-wspace)
(require 'init-evil)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-helm)
(require 'init-hungry-delete)
(require 'init-magit)
(require 'init-mc)
(require 'init-powerline)
(require 'init-projectile)
(require 'init-rainbow-mode)
(require 'init-undo-tree)
(require 'init-uniquify)
(require 'init-semantic)
(require 'init-shell-pop)
(require 'init-smartparens)
(require 'init-smex)
(require 'init-trac-wiki)
(require 'init-yasnippet)
;; programming languages
(require 'init-programming)
(require 'init-android)
(require 'init-c-mode-common)
(require 'init-c)
(require 'init-emacs-lisp)
(require 'init-java)
(require 'init-javascript)
(require 'init-latex)
(require 'init-slime)

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(magit-use-overlays nil)
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

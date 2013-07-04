;;; init.el --- Starting point for Alex Murray's Emacs Configuration

;;; Commentary:
;;

;;; Code:
(add-to-list 'load-path user-emacs-directory)

(require 'init-packages)
(require 'init-misc)
(require 'init-editing)
(require 'init-ui)
(require 'init-ido)
(require 'init-semantic)
(require 'init-uniquify)
;; vendor packages
(require 'init-vendor)
;; external packages from elpa / marmalade
(require 'init-ace-jump-mode)
(require 'init-company-mode)
(require 'init-diminish)
(require 'init-evil)
(require 'init-expand-region)
(require 'init-fill-column-indicator)
(require 'init-fullscreen-mode)
(require 'init-git-gutter)
(require 'init-magit)
(require 'init-mc)
(require 'init-rainbow-mode)
(require 'init-undo-tree)
(require 'init-uniquify)
(require 'init-paredit)
(require 'init-semantic)
(require 'init-slime)
(require 'init-smex)
(require 'init-yasnippet)
;; programming languages
(require 'init-programming)
(require 'init-android)
(require 'init-c-mode-common)
(require 'init-c)
(require 'init-emacs-lisp)
(require 'init-javascript)
(require 'init-latex)
(require 'init-python)

(provide 'init)

;;; init.el ends here

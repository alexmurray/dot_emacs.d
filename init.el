;;; init.el --- Starting point for Alex Murray's Emacs Configuration

;;; Commentary:
;;

;;; Code:
(add-to-list 'load-path user-emacs-directory)

(require 'init-packages)
(require 'init-misc)
(require 'init-editing)
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
(require 'init-anzu)
(require 'init-auto-complete)
(require 'init-diff-hl)
(require 'init-diminish)
(require 'init-dsvn)
(require 'init-elpy)
(require 'init-ethan-wspace)
(require 'init-evil)
(require 'init-expand-region)
(require 'init-helm)
(require 'init-magit)
(require 'init-mc)
(require 'init-projectile)
(require 'init-rainbow-mode)
(require 'init-undo-tree)
(require 'init-uniquify)
(require 'init-semantic)
(require 'init-shell-pop)
(require 'init-smart-mode-line)
(require 'init-smartparens)
(require 'init-smex)
(require 'init-tracwiki-mode)
(require 'init-yasnippet)
(require 'init-zeitgeist)
;; programming languages
(require 'init-programming)
(require 'init-android)
(require 'init-c-mode-common)
(require 'init-c)
(require 'init-emacs-lisp)
(require 'init-java)
(require 'init-javascript)
(require 'init-latex)

(provide 'init)

;;; init.el ends here

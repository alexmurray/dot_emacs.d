;;; init-packages.el --- Initialises and installs required packages

;;; Commentary:
;;

(require 'package)
;;; Code:
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages
  '(ac-slime ace-jump-mode android-mode auctex auto-complete
	     auto-complete-clang autopair c-eldoc change-inner
	     color-theme-sanityinc-tomorrow diminish evil evil-leader
	     evil-numbers evil-paredit evil-nerd-commenter
	     expand-region fill-column-indicator flycheck
	     fullscreen-mode fuzzy git-commit-mode gitconfig-mode
	     gitignore-mode git-gutter ido-ubiquitous js2-mode magit
	     multiple-cursors paredit php-mode rainbow-mode scratch
	     smartparens smex smooth-scroll surround undo-tree
	     yasnippet))

(when (null package-archive-contents)
  (message "%s" "Updating packages...")
  (package-refresh-contents)
  (message "%s" "done."))

;; see if all packages are installed
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'init-packages)

;;; init-packages.el ends here

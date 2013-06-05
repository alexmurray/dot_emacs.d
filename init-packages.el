;;; init-packages.el --- Initialises and installs required packages

;;; Commentary:
;;

;;; Code:
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar apm-packages
  '(ac-slime ace-jump-mode android-mode auctex auto-complete
	     auto-complete-clang autopair c-eldoc
	     color-theme-sanityinc-tomorrow diminish evil evil-leader
	     evil-numbers evil-paredit evil-nerd-commenter
	     expand-region fill-column-indicator flycheck flx
	     fullscreen-mode fuzzy git-commit-mode gitconfig-mode
	     gitignore-mode git-gutter ido-ubiquitous js2-mode magit
	     multiple-cursors paredit php-mode rainbow-mode scratch
	     smartparens smex smooth-scroll surround undo-tree
	     yasnippet))

(defun apm-packages-installed-p ()
  "Check if all packages in `apm-packages' are installed."
  (every #'package-installed-p apm-packages))

(defun apm-install-packages ()
  "Install all packages listed in `apm-packages'."
  (unless (apm-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
     (remove-if #'package-installed-p apm-packages))))

(apm-install-packages)

(provide 'init-packages)

;;; init-packages.el ends here

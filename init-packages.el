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

(defvar apm-packages '(ac-slime ack-and-a-half ace-jump-mode
   android-mode anzu auctex auto-complete auto-complete-clang
   c-eldoc color-theme-sanityinc-tomorrow diff-hl diminish dsvn
   ethan-wspace evil evil-leader evil-matchit evil-numbers
   expand-region flycheck flx-ido fuzzy git-commit-mode
   gitconfig-mode gitignore-mode helm helm-projectile
   ido-ubiquitous js2-mode latex-extra magit multiple-cursors
   php-mode projectile rainbow-mode scratch shell-pop smartparens
   smex smooth-scroll surround undo-tree xml-rpc yasnippet
   zeitgeist))

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

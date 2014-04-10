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

(defvar apm-packages '(ack-and-a-half
                       ace-jump-mode
                       adaptive-wrap
                       android-mode
                       anaconda-mode
                       anzu
                       auctex
                       company
                       deferred
                       diff-hl
                       diminish
                       dsvn
                       ethan-wspace
                       evil
                       evil-leader
                       evil-matchit
                       evil-nerd-commenter
                       evil-numbers
                       evil-visualstar
                       expand-region
                       flycheck
                       flycheck-pos-tip
                       flx-ido
                       fuzzy
                       git-commit-mode
                       gitconfig-mode
                       gitignore-mode
                       helm
                       helm-projectile
                       ido-vertical-mode
                       ido-ubiquitous
                       js2-mode
                       magit
                       multiple-cursors
                       php-mode
                       projectile
                       rainbow-mode
                       scratch
                       shell-pop
                       smartparens
                       smex
                       smooth-scroll
                       solarized-theme
                       surround
                       undo-tree
                       xml-rpc
                       yasnippet
                       zeitgeist
                       zenburn-theme))

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

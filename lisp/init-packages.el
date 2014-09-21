;;; init-packages.el --- Initialises and installs required packages

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(require 'package)

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
                       browse-kill-ring
                       c-eldoc
                       column-enforce-mode
                       company
                       company-auctex
                       deferred
                       diff-hl
                       diminish
                       dsvn
                       elisp-slime-nav
                       ethan-wspace
                       evil
                       evil-args
                       evil-jumper
                       evil-leader
                       evil-nerd-commenter
                       evil-numbers
                       evil-space
                       evil-surround
                       evil-visualstar
                       exec-path-from-shell
                       expand-region
                       flycheck
                       flycheck-pos-tip
                       flycheck-color-mode-line
                       flx-ido
                       fuzzy
                       git-commit-mode
                       gitconfig-mode
                       gitignore-mode
                       helm
                       helm-projectile
                       hungry-delete
                       ido-vertical-mode
                       ido-ubiquitous
                       js2-mode
                       magit
                       multiple-cursors
                       php-mode
                       powerline
                       projectile
                       rainbow-mode
                       color-theme-sanityinc-tomorrow
                       scratch
                       shell-pop
                       smartparens
                       smex
                       undo-tree
                       xml-rpc
                       yasnippet))

(defun apm-packages-installed-p ()
  "Check if all packages in `apm-packages' are installed."
  (cl-every #'package-installed-p apm-packages))

(defun apm-install-packages ()
  "Install all packages listed in `apm-packages'."
  (unless (apm-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
     (cl-remove-if #'package-installed-p apm-packages))))

(apm-install-packages)

(provide 'init-packages)

;;; init-packages.el ends here

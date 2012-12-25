;;; init-misc.el --- Miscellaneous settings
;; customizations

;;; Commentary:
;;

;;; Code:

(setq user-full-name "Alex Murray")
(setq user-mail-address "murray.alex@gmail.com")

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; use chrome as default broswer
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; since we store .emacs in a symlinked git repo, always follow symlinks for vc
(setq vc-follow-symlinks t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; use webjump to search websites quickly
(require 'webjump)
(global-set-key (kbd "C-x w") 'webjump)

(provide 'init-misc)

;;; init-misc.el ends here

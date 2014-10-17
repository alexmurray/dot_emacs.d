;;; init-misc.el --- Miscellaneous settings
;; customizations

;;; Commentary:
;;

;;; Code:

(setq user-full-name "Alex Murray")
(setq user-mail-address "alexmurray@fastmail.fm")

;; some emacs 24.4 specific options
;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)
;; enable eldoc in eval-expression
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;;
;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; use firefox as default broswer
(setq-default browse-url-browser-function 'browse-url-generic
              browse-url-generic-program "firefox")

;; since we store .emacs in a symlinked git repo, always follow symlinks for vc
(setq vc-follow-symlinks t)

;; ediff options to not use a second window and split properly
(require 'ediff)
(autoload 'ediff-files "ediff")
(autoload 'ediff-buffers "ediff")
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; save minibuffer history
(require 'savehist)
(savehist-mode 1)

;; use webjump to search websites quickly
(require 'webjump)
(require 'bind-key)
(bind-key "C-x w" 'webjump)

(require 'notifications nil t)
(defun apm-notify (msg)
  "Notify user of MSG using desktop notification or (message)."
  (if (eq system-type 'gnu/linux)
      (notifications-notify :body msg)
    (message msg)))

(defun apm-camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(when (version< emacs-version "24.4")
  (apm-notify "Emacs version too old - please run 24.4 or newer"))

(provide 'init-misc)

;;; init-misc.el ends here

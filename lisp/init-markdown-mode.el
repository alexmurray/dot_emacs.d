;;; init-markdown-mode.el --- Initialise the markdown-mode package

;;; Commentary:
;;

;;; Code:

(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(unless (executable-find markdown-command)
  (apm-notify "markdown not found - is it installed?"))

(provide 'init-markdown-mode)

;;; init-markdown-mode.el ends here

;;; init-uniquify.el --- Initialise uniquify

;;; Commentary:
;;

(require 'uniquify)
;;; Code:

(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(provide 'init-uniquify)

;;; init-uniquify.el ends here

;;; init-yasnippet.el --- Initialise the yasnippet package
;; yasnippet

;;; Commentary:
;; 

;;; Code:
(require 'yasnippet)
(setq yas/snippet-dirs (expand-file-name (concat user-emacs-directory "snippets")))
;; enable yasnippet globally
(yas/global-mode 1)

(eval-after-load "yasnippet"
  '(eval-after-load "diminish"
     '(diminish 'yas-minor-mode " Y")))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here

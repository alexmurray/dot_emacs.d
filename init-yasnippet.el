;;; init-yasnippet.el --- Initialise the yasnippet package
;; yasnippet

;;; Commentary:
;; 

;;; Code:
(require 'yasnippet)
;; set this first so we don't get the bundled snippets loaded since
;; they don't generally match my desired style / indentation etc
(setq yas/snippet-dirs (expand-file-name (concat user-emacs-directory "snippets")))
;; enable yasnippet globally
(yas/global-mode 1)

(eval-after-load "yasnippet"
  '(eval-after-load "diminish"
     '(diminish 'yas-minor-mode " Y")))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here

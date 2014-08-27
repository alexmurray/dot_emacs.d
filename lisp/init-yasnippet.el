;;; init-yasnippet.el --- Initialise the yasnippet package
;; yasnippet

;;; Commentary:
;;

;;; Code:
(require 'yasnippet)
;; set this first so we don't get the bundled snippets loaded since
;; they don't generally match my desired style / indentation etc
(setq yas/snippet-dirs (expand-file-name "snippets" user-emacs-directory))
;; enable yasnippet globally
(yas/global-mode 1)

;; diminish to cut / scissors icon from fontawesome
(eval-after-load 'diminish
  '(diminish 'yas-minor-mode (concat " " [#xF0C4])))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here

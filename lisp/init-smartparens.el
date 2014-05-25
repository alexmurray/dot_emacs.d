;;; init-smartparens.el --- Initialise smartparens package

;;; Commentary:
;;

;;; Code:

;; use the default smartparens config
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
;; always jump out of string when hitting end "
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

;; highlights matching pairs
(show-smartparens-global-mode +1)

;; disable pairing of ' in minibuffer
(sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

(eval-after-load 'diminish
  '(diminish 'smartparens-mode " ()"))

(provide 'init-smartparens)

;;; init-smartparens.el ends here

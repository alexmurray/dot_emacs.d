;;; init-auto-complete.el --- Initialise the auto-complete package
;; auto-complete

;;; Commentary:
;;

;;; Code:

(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)

(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)
(setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev
                           ac-source-dictionary
                           ac-source-words-in-same-mode-buffers))
(setq ac-auto-show-menu (+ ac-delay 0.1)) ; show menu after 100ms
;; quick help has to be after menu so again set to 100ms more
(setq ac-quick-help-delay (+ ac-auto-show-menu 0.1))

;; initialise ispell backend for auto-complete
(ac-ispell-setup)

(eval-after-load "auto-complete"
  (eval-after-load "diminish"
    '(diminish 'auto-complete-mode " AC")))

(provide 'init-auto-complete)

;;; init-auto-complete.el ends here

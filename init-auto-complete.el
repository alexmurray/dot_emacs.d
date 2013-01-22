;;; init-auto-complete.el --- Initialise the auto-complete package
;; auto-complete

;;; Commentary:
;;

;;; Code:

(require 'auto-complete-clang)
(setq clang-completion-suppress-error 't)

(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start 1) ; make autostart after entering a single character
(setq ac-auto-show-menu (+ ac-delay 0.1)) ; show menu after 100ms
;; quick help has to be after menu so again set to 100ms more
(setq ac-quick-help-delay (+ ac-auto-show-menu 0.1))

(eval-after-load "auto-complete"
  (eval-after-load "diminish"
    '(diminish 'auto-complete-mode " A")))

(provide 'init-auto-complete)

;;; init-auto-complete.el ends here

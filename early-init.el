;;; early-init --- Early initialization
;;; Commentary:
;;; Code:
;; Used in emacs 27 to speed up initial package loading
(setq package-quickstart t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; use the system monospace font
(setq-default font-use-system-font t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here

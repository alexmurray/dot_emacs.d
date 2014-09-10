;;; init-gdb.el --- Customise gdb integration
;; customise debugging integration

;;; Commentary:
;;

;;; Code:

(require 'gud)
(require 'gdb-mi)
;; use gdb-many-windows by default
(setq gdb-many-windows t)
;; Non-nil means display source file containing the main routine at startup
(setq gdb-show-main t)

(defun apm-enable-gud-tooltip-mode ()
  "Enable `'gud-tooltip-mode'."
  ;; enable tooltips in gud mode buffer
  (gud-tooltip-mode t))

(add-hook 'gud-mode-hook 'apm-enable-gud-tooltip-mode)

(provide 'init-gdb)

;;; init-gdb.el ends here

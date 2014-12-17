;;; init-ui.el --- Initialises the frame and font settings etc

;;; Commentary:
;;

;;; Code:

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; disable menu, tool and scroll-bars, show time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
(display-time-mode 1)
;; Show line column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

(defun apm-graphic-frame-init ()
  "Initialise properties specific to graphical display."
  (interactive)
  (when (display-graphic-p)
    (setq frame-title-format '(buffer-file-name "%f" ("%b")))
    ;; don't use gtk style tooltips so instead can use pos-tip etc
    (custom-set-variables
     '(x-gtk-use-system-tooltips nil))
    (tooltip-mode -1)
    (mouse-wheel-mode t)
    (blink-cursor-mode -1)
    (if (font-info "Ubuntu Mono")
        (set-face-attribute 'default nil :font "Ubuntu Mono 12")
      (apm-notify "Ubuntu Mono font is not installed."))
    (unless (font-info "FontAwesome")
      (apm-notify "FontAwesome is not installed."))))

;; make sure graphical properties get set on client frames
(add-hook 'server-visit-hook 'apm-graphic-frame-init)

(apm-graphic-frame-init)

;; prettify symbols (turn lambda -> λ)
(global-prettify-symbols-mode 1)

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; theme
(load-theme 'solarized-light t)

(provide 'init-ui)

;;; init-ui.el ends here

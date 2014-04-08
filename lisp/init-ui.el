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
(display-time-mode 1)
;; Show line column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

;; blink cursor
(blink-cursor-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (set-face-attribute 'default nil :font "Ubuntu Mono 12"))

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; pretty lambda (see also slime) ->  "λ"
;;  'greek small letter lambda' / utf8 cebb / unicode 03bb -> \u03BB / mule?!
;; in greek-iso8859-7 -> 107  >  86 ec
(defun font-lock-pretty-lambdas ()
  "Add font-lock keywords to replace (\lambda to λ in the current mode."
  (font-lock-add-keywords
   nil `(("(\\(\\lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    'font-lock-keyword-face))))))



;; toggle fullscreen in X
(defun toggle-fullscreen ()
  "Toggle fullscreen in X."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

;; theme
(load-theme 'solarized-light t)

(provide 'init-ui)

;;; init-ui.el ends here

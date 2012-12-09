;;; init-ui.el --- Initialises the frame and font settings etc

;;; Commentary:
;;

;;; Code:

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; disable tool-bar and scroll-bar, show matching parenthesis and time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(display-time)
;; Show line column numbers in mode line
(line-number-mode t)
(column-number-mode t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  ;; (set-face-attribute 'default nil :font "Ubuntu Mono 12")
  )

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; make f11 full-screen - from http://www.emacswiki.org/emacs/FullScreen
(defvar *old-fullscreen* nil)
(defun toggle-fullscreen ()
  "Toggle the current frame between fullscreen and normal."
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     *old-fullscreen*
			   (progn (setq *old-fullscreen* current-value)
				  'fullboth)))))
(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; pretty lambda (see also slime) ->  "λ"
;;  'greek small letter lambda' / utf8 cebb / unicode 03bb -> \u03BB / mule?!
;; in greek-iso8859-7 -> 107  >  86 ec
(defun font-lock-pretty-lambdas ()
  "Add font-lock keywords to replace (\lambda to λ. in the current mode."
  (font-lock-add-keywords
   nil `(("(\\(\\lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    'font-lock-keyword-face))))))



;; theme
(load-theme 'sanityinc-tomorrow-night t)

(provide 'init-ui)

;;; init-ui.el ends here

;;; init-fill-column-indicator.el --- Initialise fill-column-indicator

;;; Commentary:
;;

;;; Code:
(require 'fill-column-indicator)
;; use fill-column-indicator in all programming buffers - taken from
;; https://github.com/purcell/emacs.d/blob/master/init-editing-utils.el#L151
(defun prog-mode-setup-fci ()
  (turn-on-fci-mode)
  ;; workaround fci-mode not working correctly with just
  ;; show-trailing-whitespace and instead manually use whitespace
  ;; package to show trailing whitespace when using fci-mode
  (when show-trailing-whitespace
    (set (make-local-variable 'whitespace-style) '(face trailing))
    (whitespace-mode 1)))
(add-hook 'prog-mode-hook 'prog-mode-setup-fci)
(provide 'init-fill-column-indicator)

;;; init-fill-column-indicator.el ends here

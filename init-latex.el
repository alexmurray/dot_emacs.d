;;; init-latex.el --- Initialise LaTeX
;; LaTeX

;;; Commentary:
;;

;;; Code:

;; standard auctex setup
(eval-after-load "latex"
  '(progn
     (setq-default TeX-auto-save t)
     (setq-default TeX-parse-self t)
     (setq-default TeX-PDF-mode t)
     (setq-default TeX-master nil)
     (setq-default reftex-plug-into-AUCTeX t)
     (setq-default TeX-source-specials-view-start-server t)))

(defun latex-mode-setup ()
  "Tweaks and customisations for LaTeX mode."
  ;; use visual line mode to do soft word wrapping
  (visual-line-mode 1)
  ;; Enable flyspell
  (flyspell-mode 1)
  ;; use flycheck for on the fly syntax checking
  (flycheck-mode 1)
  ;; use ac-ispell for auto-complete
  (require 'ac-ispell)
  (ac-ispell-ac-setup)
  ;; smartparens latex support
  (require 'smartparens-latex)
  ;; Enable source-specials for Control-click forward/reverse search.
  (TeX-source-specials-mode 1)
  ;; enable math mode in latex
  (LaTeX-math-mode 1)
  ;; Enable reftex
  (turn-on-reftex))

(add-hook 'LaTeX-mode-hook 'latex-mode-setup)

(provide 'init-latex)

;;; init-latex.el ends here

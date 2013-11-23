;;; init-latex.el --- Initialise LaTeX
;; LaTeX

;;; Commentary:
;;

;;; Code:

;; standard auctex setup
(eval-after-load "auctex"
  '(progn
     (setq TeX-auto-save t)
     (setq TeX-parse-self t)
     (setq TeX-PDF-mode t)
     (setq-default TeX-master nil)
     ;; enable math mode in latex
     (LaTeX-math-mode 1)
     ;; Enable reftex
     (turn-on-reftex)
     (setq reftex-plug-into-AUCTeX t)
     ;; Enable source-specials for Control-click forward/reverse search.
     (TeX-source-specials-mode 1)
     (setq TeX-source-specials-view-start-server t)))

(defun latex-mode-setup ()
  "Tweaks and customisations for LaTeX mode."
  ;; use visual line mode to do soft word wrapping
  (visual-line-mode 1)
  ;; Enable flyspell
  (flyspell-mode 1)
  ;; use flycheck for on the fly syntax checking
  (flycheck-mode t)
  ;; use ac-ispell for auto-complete
  (require 'ac-ispell)
  (ac-ispell-ac-setup)
  ;; smartparens latex support
  (require 'smartparens-latex))

(add-hook 'LaTeX-mode-hook 'latex-mode-setup)

(provide 'init-latex)

;;; init-latex.el ends here

;;; init-c-mode-common.el --- Initialise common c modes
;; C
;; show #if 0 / #endif etc regions in comment face - taken from
;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa

;;; Commentary:
;; 

;;; Code:

(defun c-mode-font-lock-if0 (limit)
  "Fontify #if 0 / #endif as comments for c modes etc.
Bound search to LIMIT as a buffer position to find appropriate
code sections."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

;; c-mode and other derived modes (c++, java etc) etc
(defun c-mode-common-setup ()
  "Tweaks and customisations for all modes derived from c-common-mode."
  ;; set a reasonable fill and comment column
  (setq fill-column 78)
  (setq comment-column 70)
  ;; hide ifdef
  (hide-ifdef-mode 1)
  (eval-after-load 'diminish
    '(diminish 'hide-ifdef-mode))
  (auto-fill-mode 1)
  ;; diminish auto-fill in the modeline
  (eval-after-load 'diminish
    '(diminish 'auto-fill-function))
  ;; turn on auto-newline and hungry-delete
  (c-toggle-auto-hungry-state t)
  ;; set auto newline
  (setq c-auto-newline 1)
  ;; add doxygen support via doxymacs
  (when (require 'doxymacs nil t)
    (doxymacs-mode)
    (doxymacs-font-lock))
  ;; ensure fill-paragraph takes doxygen @ markers as start of new
  ;; paragraphs properly
  (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f")
  ;; use gtags-mode (gnu global) over ctags / etags
  (require 'gtags)
  (gtags-mode t)
  (diminish 'gtags-mode " G")
  (add-to-list 'apm-gtags-ignore-paths ".*/linux-3.0.35/")
  (add-to-list 'apm-gtags-ignore-paths ".*/linux-smx6_03/")
  (add-to-list 'apm-gtags-ignore-paths ".*/2.6.28/")
  (apm-gtags-create-or-update)

  ;; set company backends appropriately to prefer local dabbrev,
  ;; clang, gtags, semantic, yasnippet
  (eval-after-load "company"
    '(setq company-backends '((company-semantic company-clang company-gtags company-dabbrev-code company-yasnippet))))
  ;; show #if 0 / #endif etc regions in comment face
  (font-lock-add-keywords
   nil
   '((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'c-mode-common-setup)

;; use smartparens to indent new blocks correctly
(defun apm-c-mode-common-open-block (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(eval-after-load 'smartparens
  (dolist (mode '(c-mode c++-mode java-mode))
    ;; use smartparens to automatically indent correctly when opening
    ;; a new block
    (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET")))))

(provide 'init-c-mode-common)

;;; init-c-mode-common.el ends here

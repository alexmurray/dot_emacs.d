;;; init-c.el --- Initialise c mode
;; c-only modes

;;; Commentary:
;;

;;; Code:

(defun c-mode-setup ()
  "Tweaks and customisations for `c-mode'."
  ;; use semantic as source for auto complete
  (add-to-list 'ac-sources 'ac-source-semantic)
  ;; use linux kernel and hence GNOME coding style for C
  (c-set-style "linux")
  ;; and treat linux style as safe for local variable
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux))
  ;; use c-eldoc from vendor
  (add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))
  (require 'c-eldoc)
  ;; turn on c-eldoc
  (c-turn-on-eldoc-mode)
  ;; enable gtk-doc helpers from gtk-doc-tools to easily
  ;; insert gtk-doc style comment declarations using C-x 4 h
  ;; (gtk-doc-insert) or C-x 4 s (gtk-doc-insert-section) to
  ;; comment current function or section respectively
  (load "gtk-doc" t) ; ignore error if can't be found
  ;; devhelp - ignore error if couldn't be loaded
  (when (require 'devhelp nil t)
    ;; reduce timeout to look up faster
    (setq devhelp-assistant-timeout 0.2)
    ;; Bind F6 to enable the automatic assistant.
    (global-set-key (kbd "<f6>") 'devhelp-toggle-automatic-assistant)
    ;; Bind F7 to search with the assistant window.
    (global-set-key (kbd "<f7>") 'devhelp-assistant-word-at-point)))
(add-hook 'c-mode-hook 'c-mode-setup)

(provide 'init-c)

;;; init-c.el ends here

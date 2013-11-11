;;; init-c.el --- Initialise c mode
;; c-only modes

;;; Commentary:
;;

;;; Code:

(defvar include-guard-format "__%s__"
  "Format string for include guard.
%s replaced by uppercase of filename with - and . replaced by _
So if the filename is foo-bar.h the replacement would be FOO_BAR_H
and the default include guard would be __FOO_BAR_H__.")

(defun include-guard-for-filename (filename)
  "Return a suitable C/C++ include guard derived from FILENAME."
  (let* ((fbasename (replace-regexp-in-string ".*/" "" filename))
         (inc-guard-base (replace-regexp-in-string "[.-]"
                                                   "_"
                                                   fbasename)))
    (format include-guard-format (upcase inc-guard-base))))

(defun add-include-guard-if-header-file ()
  "Insert include guard if current buffer filename ends with .h."
  (let ((filename (buffer-file-name (current-buffer))))
    (when (string= ".h" (substring filename -2))
      (let ((include-guard (include-guard-for-filename filename)))
	(insert "#ifndef " include-guard)
	(newline)
	(insert "#define " include-guard)
	(newline 4)
	(insert "#endif")
	(newline)
	(forward-line -3)
	(set-buffer-modified-p nil)))))

;; when creating a new .h file automatically insert an appropriate
;; include gaurd
(add-hook 'find-file-not-found-hooks
	  'add-include-guard-if-header-file)

(defun c-mode-setup ()
  "Tweaks and customisations for `c-mode'."
  ;; use semantic and yasnippet as only sources for auto complete
  ;; since abbrev and words-in-same-buffer pollute the list to much -
  ;; semantic should pick up global etc as well so no need to use that
  (setq ac-sources '(ac-source-gtags ac-source-semantic
                     ac-source-semantic-raw ac-source-yasnippet))
  ;; use Cohda style for C which is ellemtel with 2 spaces
  (c-set-style "ellemtel")
  (setq c-basic-offset 2)
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

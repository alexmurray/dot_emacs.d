;;; init-c.el --- Initialise c mode
;; c-only modes

;;; Commentary:
;;

;;; Code:

(defun apm-c-type-to-format (type)
  "Return a printf format directive for TYPE."
  (cond ((or (null type)
             (string= type ""))
         nil)
        ((string-match "^[[:space:]]*char \\*[[:space:]]*$" type)
         "%s")
        ((string-match "\\*" type)
         "%p")
        ((cl-some #'(lambda (r) (string-match r type))
                 '("u\\(char\\|int\\|long\\)" "unsigned"))
         "%u")
        ((cl-some #'(lambda (r) (string-match r type))
                 '("\\(char\\|int\\|long\\)"))
         "%d")
        ((cl-some #'(lambda (r) (string-match r type))
                 '("\\(float\\|double\\)"))
         "%f")
        ;; use an invalid printf directive if none found
        (t "%?")))

(defvar apm-c-variable-decl-regex
  "\\([[:alpha:][:space:]\\*_]+[[:space:]\\*]+\\)\\([[:alpha:]_][[:alnum:]_]*\\)")

(defvar apm-c-void-regex
  "^\\([[:space:]]*\\(void\\)?[[:space:]]*\\)$")

;; useful functions for parsing out c function type arguments used in
;; c snippets for yasnippet
(defun apm-c-get-function-param-names (param-string)
  "Return a list of the parameter names from PARAM-STRING.
PARAM-STRING should be as <typename> <variable>,..."
  (unless (string-match apm-c-void-regex param-string)
    ;; get variable name from individual parameter groups
    (mapcar 'apm-c-get-variable-name-from-declaration
            (split-string param-string ","))))

(defun apm-c-get-function-param-types (param-string)
  "Return a list of the parameter types from PARAM-STRING.
PARAM-STRING should be as <typename> <variable>,..."
  (unless (string-match apm-c-void-regex param-string)
    ;; get variable type from individual parameter groups
    (mapcar 'apm-c-get-variable-type-from-declaration
            (split-string param-string ","))))

(defun apm-c-get-function-param-formats (param-string)
  "Return a list of the parameter printf formats from PARAM-STRING.
PARAM-STRING should be as <typename> <variable>,..."
  (mapcar 'apm-c-type-to-format (apm-c-get-function-param-types param-string)))

(defun apm-c-get-variable-name-from-declaration (decl-string)
  "Return the name of the variable declared in DECL-STRING."
  (if (string-match apm-c-variable-decl-regex decl-string)
      (match-string 2 decl-string)
    nil))

(defun apm-c-get-variable-type-from-declaration (decl-string)
  "Return the type of the variable declared in DECL-STRING."
  (if (string-match apm-c-variable-decl-regex decl-string)
      (match-string 1 decl-string)
    nil))

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

;; define a specific Cohda C indentation style - inherit from ellemtel
;; but use 2 spaces, no tabs and no offset for labels
(defconst cohda-c-style
  '("ellemtel"
    (indent-tabs-mode . nil)
    (c-basic-offset . 2)
    (c-offsets-alist . ((label . 0))))
  "Cohda C Programming Style.")

(c-add-style "cohda" cohda-c-style)

(defun c-mode-setup ()
  "Tweaks and customisations for `c-mode'."
  (c-set-style "cohda")
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
    (require 'bind-key)
    (bind-key "<f6>" 'devhelp-toggle-automatic-assistant)
    ;; Bind F7 to search with the assistant window.
    (bind-key "<f7>" 'devhelp-assistant-word-at-point))
  ;; ensure fill-paragraph takes doxygen @ markers as start of new
  ;; paragraphs properly
  (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
(add-hook 'c-mode-hook 'c-mode-setup)

(provide 'init-c)

;;; init-c.el ends here

;;; apm-c.el --- My customisations for c mode
;; c-only modes

;;; Commentary:
;;

;;; Code:

(require 'cohda-c)

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

(defun apm-c-mode-setup ()
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
  ;;(load "gtk-doc" t) ; ignore error if can't be found
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

(add-hook 'c-mode-hook 'apm-c-mode-setup)

(provide 'apm-c)

;;; apm-c.el ends here

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
  ;; ensure fill-paragraph takes doxygen @ markers as start of new
  ;; paragraphs properly
  (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))

(add-hook 'c-mode-hook 'apm-c-mode-setup)

(provide 'apm-c)

;;; apm-c.el ends here

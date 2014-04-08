;;; init-zeitgeist.el --- Initialise the zeitgeist package

;;; Commentary:
;; 

;;; Code:
(require 'zeitgeist)
(require 's)

(defvar zeitgeist-ignored-regexs
  '(".ido.last"
    ".recentf"
    "COMMIT_EDITMSG"
    ;; TODO - we still don't seem to catch the flycheck- files events
    "flycheck-{file}"
    "git-rebase-todo")
  "List of filename regex's to ignore for zeitgeist.

{file} is replaced by the name of the file.")

;; ignore a bunch of different files which are not useful
(defadvice zeitgeist-send (around zeitgeist-send-ignored activate)
  "Don't send zeitgeist events for files which match ZEITGEIST-IGNORED-FILES."
  ;; zeitgeist-send has &rest arguments using :file for filename
  (let ((file (plist-get (ad-get-args 1) :file))
        (ignored))
    (dolist (re zeitgeist-ignored-regexs)
      (unless ignored
        ;; replace {file} in regexp with file
        (setq re (replace-regexp-in-string
                  "{file}" (file-name-nondirectory file) re))
        (setq ignored (s-ends-with? re file))))
    (if ignored
        (message "Ignored zeitgeist event for file: %s" file)
      ad-do-it)))

(provide 'init-zeitgeist)

;;; init-zeitgeist.el ends here

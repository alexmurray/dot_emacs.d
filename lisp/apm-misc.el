;;; apm-misc --- Miscellaneous functions

;;; Commentary:

;;; Code:

(defun apm-browse-lp-bug-at-point ()
  "Browse to the launchpad bug referenced by bug number at point."
  (interactive)
  (let ((base-uri "https://bugs.launchpad.net/bugs/")
        (id (substring-no-properties (thing-at-point 'symbol))))
    (when id
      (browse-url (concat base-uri id)))))

(defvar apm-cve-regex "\\(CVE-[0-9]\\{4\\}-[0-9]+\\)")

(defun apm-cve-at-point ()
  "Return the CVE at point or nil if none found."
  (let ((cve (thing-at-point 'symbol t)))
    (when (or (null cve) (not (string-match apm-cve-regex cve)))
      (save-excursion
        (let ((end (save-excursion (forward-line) (point))))
          (backward-word)
          (if (re-search-forward apm-cve-regex end t)
              (setq cve (substring-no-properties (match-string 1)))
            (setq cve nil)))))
    cve))

(defun apm-find-uct-cve (&optional cve)
  "Open the specified CVE file in UCT (or if no CVE use the one at point)."
  (interactive (list (or (apm-cve-at-point) (read-string "CVE: "))))
  (let ((uct-path (expand-file-name "~/ubuntu/git/ubuntu-cve-tracker/"))
        (dirs '("active" "retired" "ignored" "embargoed")))
    (when (and cve (string-match apm-cve-regex cve))
      (let ((cve-file (car (cl-remove-if #'null
                                         (mapcar #'(lambda (dir)
                                                     (let ((file (format "%s/%s/%s" uct-path dir cve)))
                                                       (when (file-exists-p file)
                                                         file)))
                                                 dirs)))))
        (if cve-file
            (find-file-other-window cve-file)
          (user-error "Unable to find %s in UCT" cve))))))

(provide 'apm-misc)
;;; apm-misc.el ends here

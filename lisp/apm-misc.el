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

(defun apm-find-uct-cve-at-point ()
  "Open the CVE file in UCT at point."
  (interactive)
  (let ((uct-path (expand-file-name "~/ubuntu/git/ubuntu-cve-tracker/"))
        (dirs '("active" "retired" "ignored"))
        (cve (substring-no-properties (thing-at-point 'symbol))))
    (when (and cve (string-match "\\(CVE-[0-9]\\{4\\}-[0-9]+\\)" cve))
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

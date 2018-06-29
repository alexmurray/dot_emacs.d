(defun apm-browse-lp-bug-at-point ()
  "Browse to the launchpad bug referenced by bug number at point."
  (interactive)
  (let ((base-uri "https://bugs.launchpad.net/bugs/")
        (id (thing-at-point 'symbol)))
    (when id
      (browse-url (concat base-uri id)))))

(provide 'apm-misc)
;;; apm-misc.el ends here

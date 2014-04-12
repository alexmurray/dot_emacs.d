;;; init-gtags.el --- Initiali se gtags

;;; Commentary:
;;

;;; Code:

;; don't explicity require gtags as may not be installed
(unless (require 'gtags nil t)
  (message "gtags support not available - is GNU Global installed?"))

;; stop gtags.el stealing middle mouse click paste
(eval-after-load 'gtags
  '(define-key gtags-mode-map [mouse-2] 'mouse-yank-primary))

(defun apm-gtags-create-or-update ()
  "Create or update the GNU Global tag file."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    ;; check is not ignored
    (when (cl-notany '(lambda (path) (string-match path dir))
                     apm-gtags-ignore-paths)
      (if (not (= 0 (call-process "global" nil nil nil " -p")))
          ;; tagfile does not exist yet - prompt for where to create one
          (let* ((dir (read-directory-name
                       "gtags: top of source tree:" default-directory))
                 (cmd (format
                       "cd %s && gtags && echo 'Created GNU Global tag file'"
                       dir))
                 (process (start-process-shell-command "gtags-create" nil
                                                       cmd)))
            ;; display message about creating tags file
            (message "Creating global tags file...")
            (set-process-sentinel process
                                  '(lambda (process event)
                                     (message "Finished creating global tags file: %s" event))))
        ;; tagfile already exists - update it
        (let ((process (start-process-shell-command "gtags-update" nil
                                                    "global -u")))
            ;; display message about updating tags file
            (message "Updating global tags file...")
            (set-process-sentinel process
                                  '(lambda (process event)
                                     (message "Finished updating global tags file: %s" event))
))))))

(defvar apm-gtags-ignore-paths '("/usr/")
  "A list of paths to not automatically run apm-gtags-create-or-update.")

(provide 'init-gtags)

;;; init-gtags.el ends here

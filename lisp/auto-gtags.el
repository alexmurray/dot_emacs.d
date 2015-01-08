;;; auto-gtags.el --- Automatically create / update gtags

;;; Commentary:
;;

;;; Code:

(defvar auto-gtags-ignore-paths '("/usr/")
  "A list of paths to not automatically run `auto-gtags-create-or-update'.")

(defun auto-gtags-create-or-update ()
  "Create or update the GNU Global tag file."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    ;; check is not ignored
    (when (cl-notany '(lambda (path) (string-match path dir))
                     auto-gtags-ignore-paths)
      (if (not (= 0 (call-process "global" nil nil nil " -p")))
          ;; tagfile does not exist yet - prompt for where to create
          ;; one but not if already in home directory since will index
          ;; too much
          (let ((dir (if (string= default-directory
                                  (substitute-in-file-name "$HOME/"))
                         ""
                       (read-directory-name
                        "gtags: top of source tree:" default-directory))))
            (if (string= dir "")
                (message "Not creating new global tag file.")
              (let ((process (start-process-shell-command "gtags-create" nil
                                                          (format
                                                           "cd %s && gtags && echo 'Created GNU Global tag file'"
                                                           dir))))
                ;; display message about creating tags file
                (message "Creating global tags file...")
                (set-process-sentinel process
                                      '(lambda (process event)
                                         (message "Finished creating global tags file: %s" event))))))
        ;; tagfile already exists - update it
        (let ((process (start-process-shell-command "gtags-update" nil
                                                    "global -u")))
          ;; display message about updating tags file
          (message "Updating global tags file...")
          (set-process-sentinel process
                                '(lambda (process event)
                                   (message "Finished updating global tags file: %s" event))))))))

(provide 'auto-gtags)

;;; auto-gtags.el ends here

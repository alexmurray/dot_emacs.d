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
          (let ((olddir default-directory)
                (topdir (read-directory-name
                         "gtags: top of source tree:" default-directory)))
            (cd topdir)
            (shell-command "gtags && echo 'Created GNU Global tag file'")
            (cd olddir)) ; restore
        ;; tagfile already exists - update it
        (shell-command "global -u && echo 'Updated GNU Global tagfile'")))))

(defvar apm-gtags-ignore-paths '("/usr/")
  "A list of paths to not automatically run apm-gtags-create-or-update.")

(provide 'init-gtags)

;;; init-gtags.el ends here

;;; init-android.el --- Initialise Android development stuff
;; Android (Java)

;;; Commentary:
;;

;;; Code:
(require 'android-mode)

(setq android-mode-sdk-dir "/opt/android-sdk-linux/")
;; change prefix so doesn't conflict with comment-region
(setq android-mode-key-prefix (kbd "C-c C-m"))
;; setup gud for debugging - http://gregorygrubbs.com/development/tips-on-android-development-using-emacs
(defun gud-mode-setup ()
  "Setup gud-mode for debugging during android development."
  (add-to-list 'gud-jdb-classpath (expand-file-name "~/android-sdk-linux/platforms/android-10/android.jar")))
(add-hook 'gud-mode-hook 'gud-mode-setup)

(eval-after-load 'diminish
  '(diminish 'android-mode " And"))

(provide 'init-android)

;;; init-android.el ends here

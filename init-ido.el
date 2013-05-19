;;; init-ido.el --- Initialise ido-mode
;; ido mode for opening files and switching buffers

;;; Commentary:
;;

;;; Code:

(ido-mode t)
(setq ido-enable-flex-matching t
      ;; use ido virtual buffers to remember previously opened files
      ido-use-virtual-buffers t)
;; use ido for buffer and filenames as well
(ido-everywhere t)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; ido-ubiquitous - is a separate package but let's initialise it here
(ido-ubiquitous-mode t)

(provide 'init-ido)

;;; init-ido.el ends here

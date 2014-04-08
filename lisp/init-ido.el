;;; init-ido.el --- Initialise ido-mode and related packages
;; ido mode for opening files and switching buffers

;;; Commentary:
;;

;;; Code:
(require 'ido-ubiquitous)
(require 'flx-ido)

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

;; also initialise flx package with ido
(flx-ido-mode t)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; ido-vertical-mode - is a separate package but let's initialise it here
(ido-vertical-mode t)

(provide 'init-ido)

;;; init-ido.el ends here

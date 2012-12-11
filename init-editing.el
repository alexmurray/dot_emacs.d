;;; init-editing.el --- Various editing related customisations
;; default to utf-8

;;; Commentary:
;;

;;; Code:

;; use gb dictionary via aspell if available
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
	ispell-dictionary "british"
        ispell-extra-args '("--sug-mode=ultra")))

;; use utf8 by default
(prefer-coding-system 'utf-8)

;; automatically reload buffer when file on disk changes
(global-auto-revert-mode t)

;; use CUA mode for rectangle selections etc but not copy/paste etc
(cua-selection-mode t)

;; enable delete-selection mode to allow replacing selected region
;; with new text automatically
(delete-selection-mode 1)

;; electric indent and layout modes to make more IDE like
(electric-indent-mode t)
(electric-layout-mode t)

;; show empty lines in left fringe
(setq indicate-empty-lines t)

;; show trailing whitespace in all modes
(require 'whitespace)
(setq-default whitespace-style '(face trailing))
(setq-default whitespace-line-column 80)
(global-whitespace-mode 1)

;; sentences have single spaces, not double spaces in between them -
;; http://www.slate.com/articles/technology/technology/2011/01/space_invaders.html
(setq sentence-end-double-space nil)

;; default to unified diff
(setq diff-switches "-u")

(global-set-key (kbd "C-x C-h") 'hexl-mode)
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-x m") 'eshell)

(global-set-key (kbd "C-M-h") 'backward-kill-word) ;; also like readline
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Use regex searches and replace by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Jump to a definition in the current file. (This is awesome.)
(require 'imenu)
(global-set-key (kbd "C-x C-i") 'imenu)

;; which-function-mode to display current defun in modeline
(require 'which-func)
(which-function-mode t)

;; if no mark is active then change copy / cut to do current line
;; rather than nothing to easily allow copying / cutting of lines
;; without selecting them - from
;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active
	(list (region-beginning) (region-end))
      (list (line-beginning-position) (line-beginning-position 2)))))

;; a couple nice definitions taken from emacs-starter-kit
(defun sudo-edit (&optional arg)
  "Open the current buffer (or prompt for file if ARG is non-nill) using sudo to edit as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; delete trailing whitespace on save - make sure we can toggle it
(defun enable-delete-trailing-whitespace ()
  "Delete trailing whitespace on buffer close."
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun disable-delete-trailing-whitespace ()
  "Don't delete trailing whitespace on buffer close."
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))

(provide 'init-editing)

;;; init-editing.el ends here

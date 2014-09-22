;;; init-editing.el --- Various editing related customisations
;; default to utf-8

;;; Commentary:
;;

;;; Code:

;; use gb dictionary via aspell if available
(require 'ispell)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"
        ispell-dictionary "british"
        ispell-extra-args '("--sug-mode=ultra")))

;; taken from
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;; - use abbrev to autocorrect common spelling mistakes and put spell
;; check / fix on C-x C-i
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

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

;; abbrev-mode
(abbrev-mode 1)

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

;; better buffer list
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Jump to a definition in the current file. (This is awesome.)
(require 'imenu)
(global-set-key (kbd "C-x C-i") 'imenu)

;; which-function-mode to display current defun in modeline
(require 'which-func)
(which-function-mode t)

; map return to newline and indent
(define-key global-map (kbd "RET") 'newline-and-indent)

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

;; rotate buffers around the frames in the current window - from
;; http://www.reddit.com/r/emacs/comments/1agkd6/function_to_cycle_your_current_buffers/
(defun rotate-windows (&optional reverse)
  "Rotate the windows' buffers.  If given a prefix argument, rotate in REVERSE."
  (interactive "P")
  (dolist (window (butlast (if reverse (reverse (window-list)) (window-list))))
    (let ((next-window-buffer (window-buffer (next-window window 0))))
      (set-window-buffer (next-window window 0) (window-buffer window))
      (set-window-buffer window next-window-buffer))))

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(provide 'init-editing)

;;; init-editing.el ends here

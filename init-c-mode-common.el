;;; init-c-mode-common.el --- Initialise common c modes
;; C
;; show #if 0 / #endif etc regions in comment face - taken from
;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa

;;; Commentary:
;; 

;;; Code:

(defun c-mode-font-lock-if0 (limit)
  "Fontify #if 0 / #endif as comments for c modes etc.
Bound search to LIMIT as a buffer position to find appropriate
code sections."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun apm-gtags-create-or-update ()
  "Create or update the GNU Global tag file."
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p")))
      ;; tagfile does not exist yet - prompt for where to create one
      (let ((olddir default-directory)
            (topdir (read-directory-name
                     "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (shell-command "gtags && echo 'Created GNU Global tag file'")
        (cd olddir)) ; restore
    ;; tagfile already exists - update it
    (shell-command "global -u && echo 'Updated GNU Global tagfile'")))

(defvar apm-gtags-ignore-paths
  '("/usr/"
    ".*/linux-smx6_03/"
    ".*/2.6.28/")
  "A list of paths to no automatically run apm-gtags-create-or-update.")

;; c-mode and other derived modes (c++, java etc) etc
(defun c-mode-common-setup ()
  "Tweaks and customisations for all modes derived from c-common-mode."
  ;; set a reasonable fill and comment column
  (setq fill-column 78)
  (setq comment-column 70)
  ;; make CamelCase words separate subwords (ie. Camel and Case can
  ;; be operated on separately as separate words
  (subword-mode 1)
  (auto-fill-mode 1)
  ;; diminish auto-fill in the modeline
  (diminish 'auto-fill-function " F")
  ;; turn on auto-newline and hungry-delete
  (c-toggle-auto-hungry-state t)
  ;; set auto newline
  (setq c-auto-newline 1)
  ;; use gtags-mode (gnu global) over ctags / etags
  (require 'gtags)
  (gtags-mode t)
  (diminish 'gtags-mode " G")
  (let ((dir (expand-file-name default-directory)))
    (when (cl-notany '(lambda (path) (string-match path dir)) apm-gtags-ignore-paths)
      ;; update / create a global tags db automatically
      (apm-gtags-create-or-update)))
  ;; show #if 0 / #endif etc regions in comment face
  (font-lock-add-keywords
   nil
   '((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'c-mode-common-setup)

;; use smartparens to indent new blocks correctly
(defun apm-c-mode-common-open-block (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(eval-after-load "smartparens"
  (dolist (mode '(c-mode c++-mode java-mode))
    ;; use smartparens to automatically indent correctly when opening
    ;; a new block
    (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET")))))

(provide 'init-c-mode-common)

;;; init-c-mode-common.el ends here

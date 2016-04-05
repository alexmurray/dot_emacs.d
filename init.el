;;; init.el --- Starting point for Alex Murray's Emacs Configuration

;;; Commentary:
;;

;;; Code:

;; mess with gc threshold to speed up init
(defvar apm-orig-gc-cons-threshold gc-cons-threshold)

(defun apm-restore-gc-cons-threshold ()
  "Restore `gc-cons-threshold'."
  (setq gc-cons-threshold apm-orig-gc-cons-threshold))

;; speed up init - restore back later
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'apm-restore-gc-cons-threshold)

(defun apm-max-gc-cons-threshold ()
  "Set `gc-cons-threshold' to maximum and store old value."
  (setq apm-orig-gc-cons-threshold gc-cons-threshold)
  (setq gc-cons-threshold most-positive-fixnum))

(add-hook 'minibuffer-setup-hook #'apm-max-gc-cons-threshold)
(add-hook 'minibuffer-exit-hook #'apm-restore-gc-cons-threshold)

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;; uncomment to debug package loading times
;; (setq use-package-verbose t)

;;; Package management
(require 'package)
;; we use use-package to do this for us
(setq package-enable-at-startup nil)
;; use https for both melpa and gelpa if available
(if (gnutls-available-p)
    (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			     ("melpa" . "https://melpa.org/packages/")))
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "http://melpa.org/packages/"))))

(package-initialize)

;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package alert
  :ensure t
  :init (when (eq system-type 'gnu/linux)
          (setq alert-default-style 'notifications)))

;; some useful functions for the rest of this init file
(defun apm-camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(when (version< emacs-version "24.4")
  (alert "Emacs version too old - please run 24.4 or newer"
         :severity 'high))

;;; General settings etc

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; set a reasonable fill and comment column
(setq-default fill-column 80)
(setq-default comment-column 78)

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; Make Tab complete if the line is indented
(setq tab-always-indent 'complete)

;; disable menu, tool and scroll-bars, show time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
(display-time-mode 1)
;; Show line column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)

(defun apm-emoji-fontset-init ()
  "Set fontset to display emoji correctly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
    ;; For Linux
    (if (font-info "Symbola")
        (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend)
      (alert "Symbola is not installed (ttf-ancient-fonts)"))))

(defvar apm-preferred-font-family "Inconsolata"
  "Preferred font family to use.")

(defvar apm-preferred-font-family-package "fonts-inconsolata"
  "Package to install to get `apm-preferred-font-family'.")

(defvar apm-preferred-font-height 117
  "Preferred font height to use.")

(defun apm-graphic-frame-init ()
  "Initialise properties specific to graphical display."
  (interactive)
  (when (display-graphic-p)
    (apm-emoji-fontset-init)
    (setq frame-title-format '(buffer-file-name "%f" ("%b")))
    ;; don't use gtk style tooltips so instead can use pos-tip etc
    (custom-set-variables
     '(x-gtk-use-system-tooltips nil))
    (tooltip-mode -1)
    (blink-cursor-mode -1)
    (if (font-info apm-preferred-font-family)
        (set-face-attribute 'default nil
                            :family apm-preferred-font-family
                            :height apm-preferred-font-height)
      (alert (format "%s font not installed (%s)"
                     apm-preferred-font-family
                     apm-preferred-font-family-package)))
    (if (font-info "FontAwesome")
        ;; make sure to use FontAwesome for it's range in the unicode
        ;; private use area since on Windows this doesn't happen
        ;; automagically
        (set-fontset-font "fontset-default" '(#xf000 . #xf23a) "FontAwesome")
      (alert "FontAwesome is not installed (fonts-font-awesome)."))))

;; make sure graphical properties get set on client frames
(add-hook 'server-visit-hook #'apm-graphic-frame-init)
(apm-graphic-frame-init)

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; Use regex searches and replace by default.
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-M-%" 'query-replace)

;; a couple nice definitions taken from emacs-starter-kit
(defun sudo-edit (&optional arg)
  "Open the current buffer (or prompt for file if ARG is non-nill) using sudo to edit as root."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (helm-read-file-name "File: ")))
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
(bind-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; general modes in text-mode or derived from
(defun apm-text-mode-setup ()
  "Setup `text-mode' buffers."
  ;; use visual line mode to do soft word wrapping
  (visual-line-mode 1)
  ;; and use adaptive-wrap to 'indent' paragraphs appropriately with visual-line-mode
  (adaptive-wrap-prefix-mode 1)
  ;; Enable flyspell
  (flyspell-mode 1)
  ;; give warning if words misspelled when typing
  (ispell-minor-mode 1))

(add-hook 'text-mode-hook #'apm-text-mode-setup)

;;; Packages
(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)
         ("M-p" . ace-window)))

(use-package adaptive-wrap
  :ensure t)

(use-package abbrev
  :diminish abbrev-mode
  :config (progn
            (setq save-abbrevs t)
            (setq-default abbrev-mode t)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :ensure t)

(use-package ag
  :ensure t
  :defer t
  :init (unless (executable-find "ag")
          (alert "ag not found - is it installed?")))

(use-package android-mode
  :ensure t
  :config (progn
            ;; change prefix so doesn't conflict with comment-region
            (setq android-mode-sdk-dir (expand-file-name "~/android-sdk-linux/")
                  android-mode-key-prefix (kbd "C-c C-m")))
  :diminish (android-mode . " \uf17b "))

(use-package anaconda-mode
  :ensure t
  :defer t
  :config (add-hook 'python-mode-hook #'anaconda-mode))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(use-package apm-c
  :load-path "lisp/"
  :config (dolist (hook '(c-mode-hook c++-mode-hook))
            (add-hook hook 'apm-c-mode-setup)))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package asn1-mode
  :ensure t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :init (global-auto-revert-mode t))

(defun apm-latex-mode-setup ()
  "Tweaks and customisations for LaTeX mode."
  ;; smartparens latex support
  (use-package smartparens-latex)
  ;; Enable source-correlate for Control-click forward/reverse search.
  (TeX-source-correlate-mode 1)
  ;; enable math mode in latex
  (LaTeX-math-mode 1)
  ;; Enable reftex
  (turn-on-reftex)
  ;; add company-ispell backend
  (with-eval-after-load 'company
    ;; silence byte-compilation warnings
    (eval-when-compile
      (require 'company))
    (add-to-list 'company-backends 'company-ispell)))

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init (progn
          (setq-default TeX-auto-save t)
          (setq-default TeX-parse-self t)
          (setq-default TeX-PDF-mode t)
          (setq-default TeX-master nil)
          (setq-default reftex-plug-into-AUCTeX t)
          (setq-default TeX-source-correlate-start-server t)

          (add-hook 'LaTeX-mode-hook #'apm-latex-mode-setup)))

(use-package avy
  :ensure t
  :bind ("C-c SPC" . avy-goto-char))

(use-package browse-kill-ring
  :ensure t)

(use-package bs
  :bind ("C-x C-b" . bs-show))

;; show #if 0 / #endif etc regions in comment face - taken from
;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa
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

;; c-mode and other derived modes (c++, java etc) etc
(defun apm-c-mode-common-setup ()
  "Tweaks and customisations for all modes derived from c-common-mode."
  (auto-fill-mode 1)
  ;; diminish auto-fill in the modeline
  (with-eval-after-load 'diminish
    (diminish 'auto-fill-function))
  ;; turn on auto-newline and hungry-delete
  (c-toggle-auto-hungry-state t)
  ;; ensure fill-paragraph takes doxygen @ markers as start of new
  ;; paragraphs properly
  (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f")
  ;; add key-bindings for smartparens hybrid sexps
  (with-eval-after-load 'smartparens
    (local-set-key (kbd "C-)") 'sp-slurp-hybrid-sexp)
    (local-set-key (kbd "C-<right>") 'sp-slurp-hybrid-sexp)
    (local-set-key (kbd "C-<left>") 'sp-dedent-adjust-sexp))

  ;; show #if 0 / #endif etc regions in comment face
  (font-lock-add-keywords
   nil
   '((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(use-package cc-mode
  :config (add-hook 'c-mode-common-hook #'apm-c-mode-common-setup))

(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :config (global-column-enforce-mode))

(use-package company
  :ensure t
  :commands global-company-mode
  :init (progn
          ;; set default lighter as nothing so in general it is not displayed
          ;; but will still be shown when completion popup is active to show the
          ;; backend which is in use
          (setq company-lighter-base "")
          (global-company-mode t))
  :config (progn
            ;; Use Company for completion
            (bind-key [remap completion-at-point] #'company-complete company-mode-map)

            ;; some better default values
            (setq company-idle-delay 0.5)
            (setq company-tooltip-limit 10)
            (setq company-minimum-prefix-length 2)

            ;; align annotations in tooltip
            (setq company-tooltip-align-annotations t)

            ;; nicer keybindings
            (define-key company-active-map (kbd "C-n") 'company-select-next)
            (define-key company-active-map (kbd "C-p") 'company-select-previous)
            (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)

            (define-key company-active-map [tab] 'company-complete-common-or-cycle)
            (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)

            ;; put most often used completions at stop of list
            (setq company-transformers '(company-sort-by-occurrence))))

(use-package company-anaconda
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-anaconda))

(use-package company-auctex
  :ensure t
  :defer t)

(use-package company-dabbrev
  :after company
  :config             ;; keep original case
  (setq company-dabbrev-downcase nil))

(use-package company-emoji
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-emoji))

(use-package company-flx
  :ensure t
  :after company
  :init (company-flx-mode t))

(use-package company-irony
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :after company
  :init (progn
          (setq company-irony-c-headers--compiler-executable "clang++-3.5")
          ;; group with company-irony but beforehand so we get first pick
          (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

(use-package company-math
  :ensure t
  :defer t
  :after company
  ;; Add backend for math characters
  :init (progn
          (add-to-list 'company-backends 'company-math-symbols-unicode)
          (add-to-list 'company-backends 'company-math-symbols-latex)))

(use-package company-quickhelp
  :ensure t
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.1))

(use-package company-shell
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-shell))

(use-package company-web
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-web-html))

(use-package compile
  :bind ("C-x C-m" . compile)
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error))

(use-package cstyle
  :load-path "vendor/cstyle.el")

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode t))

(use-package cus-edit
  :config (progn
            (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
            ;; load custom but ignore error if doesn't exist
            (load custom-file t)))

;; show suspicious c constructs automatically
(use-package cwarn
  :diminish cwarn-mode
  :init (global-cwarn-mode 1))

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(defun apm-devhelp-setup ()
  "Setup devhelp integration."
  (require 'devhelp)
  (local-set-key (kbd "<f6>") #'devhelp-toggle-automatic-assistant)
  (local-set-key (kbd  "<f7>") #'devhelp-assistant-word-at-point))

(use-package devhelp
  :load-path "vendor/"
  :defer t
  :init (add-hook 'c-mode-hook #'apm-devhelp-setup))

(use-package diff
  ;; default to unified diff
  :config (setq diff-switches "-u"))

(use-package diff-hl
  :ensure t
  :init (progn
          (global-diff-hl-mode)
          ;; Highlight changed files in the fringe of dired
          (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

(use-package diminish
  :ensure t)

(defun apm-doxymacs-setup()
  (doxymacs-mode)
  (doxymacs-font-lock))

(use-package doxymacs
  :defer t
  :diminish doxymacs-mode
  :config (add-hook 'c-mode-common-hook #'apm-doxymacs-setup))

(use-package dts-mode
  :ensure t)

;; taken from http://kaushalmodi.github.io/2015/03/09/do-ediff-as-i-mean/
(defun apm-ediff-dwim ()
  "Do ediff as I mean.

If a region is active when command is called, call `ediff-regions-wordwise'.

Else if the current frame has 2 windows,
- Do `ediff-files' if the buffers are associated to files and the buffers
  have not been modified.
- Do `ediff-buffers' otherwise.

Otherwise call `ediff-buffers' interactively."
  (interactive)
  (if (region-active-p)
      (call-interactively 'ediff-regions-wordwise)
    (if (= 2 (safe-length (window-list)))
        (let (bufa bufb filea fileb)
          (setq bufa  (get-buffer (buffer-name)))
          (setq filea (buffer-file-name bufa))
          (save-excursion
            (other-window 1)
            (setq bufb (get-buffer (buffer-name))))
          (setq fileb (buffer-file-name bufb))
          (if (or
               ;; if either of the buffers is not associated to a file
               (null filea) (null fileb)
               ;; if either of the buffers is modified
               (buffer-modified-p bufa) (buffer-modified-p bufb))
              (progn
                (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
                (ediff-buffers bufa bufb))
            (progn
              (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
              (ediff-files filea fileb))))
      (call-interactively 'ediff-buffers))))

(use-package ediff
  :defer t
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally))

(use-package eldoc
  :diminish eldoc-mode
  ;; enable eldoc in eval-expression
  :config (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package electric
  :init (progn
          ;; electric indent and layout modes to make more IDE like
          (electric-indent-mode t)
          (electric-layout-mode t)))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :init (progn
          (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
            (add-hook hook #'elisp-slime-nav-mode)))
  :config (with-eval-after-load 'evil
            (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "C-]")
              #'elisp-slime-nav-find-elisp-thing-at-point)))

(defun apm-erc-alert (&optional match-type nick message)
  "Show an alert when nick mentioned with MATCH-TYPE NICK and MESSAGE."
  (if (or (null match-type) (not (eq match-type 'fool)))
      (let (alert-log-messages)
        (alert (or message (buffer-string)) :severity 'high
               :title (concat "ERC: " (or nick (buffer-name)))
               :data message))))

(use-package erc
  :defer t
  :config (progn
            (setq erc-nick "alexmurray")
            ;; notify via alert when mentioned
            (add-hook 'erc-text-matched-hook 'apm-erc-alert)))


(defun apm-eshell-mode-setup ()
  "Initialise 'eshell-mode'."
  (setq mode-name "e\uF120"))

(use-package eshell
  :commands eshell
  :bind ("C-x m" . eshell)
  :config (add-hook 'eshell-mode-hook #'apm-eshell-mode-setup))

(defun makefile-tabs-are-less-evil ()
  "Disable ethan-wspace from caring about tabs in Makefile's."
  ;; silence byte-compilation warnings
  (eval-when-compile
    (require 'ethan-wspace))
  (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

(use-package ethan-wspace
  :ensure t
  :diminish ethan-wspace-mode
  :config (progn
            ;; ethan-wspace-mode raises lots of warnings if this is enabled...
            ;; hopefully this doesn't cause problems
            (setq mode-require-final-newline nil)
            ;; disable ethan-wspace caring about tabs in Makefile's
            (add-hook 'makefile-mode-hook #'makefile-tabs-are-less-evil))
  :init (global-ethan-wspace-mode 1))

(defun apm-make-underscore-word-character ()
  "Make _ a word character."
  (modify-syntax-entry ?_ "w"))

(use-package evil
  :ensure t
  :init (evil-mode t)
  :config (progn
            ;; make underscore a word character so movements across words
            ;; include it - this is the same as vim - need to do it on each
            ;; major mode change
            (add-hook 'after-change-major-mode-hook
                      #'apm-make-underscore-word-character)
            ;; make cursor easier to see
            (setq evil-normal-state-cursor '("#b294bb" box))
            (setq evil-insert-state-cursor '("#de935f" bar))
            (setq evil-emacs-state-cursor '("#cc6666" box))

            (dolist (mode '(comint-mode
                            eshell-mode
                            git-rebase-mode
                            ggtags-global-mode
                            jenkins-mode
                            jenkins-job-view-mode
                            inferior-emacs-lisp-mode
                            magit-branch-manager-mode
                            magit-popup-mode
                            magit-popup-sequence-mode
                            paradox-menu-mode
                            pylookup-mode
                            semantic-symref-results-mode
                            shell-mode
                            svn-status-mode
                            term-mode))
              (evil-set-initial-state mode 'emacs))

            ;; add vim binding for go to next misspelled word
            (with-eval-after-load 'flyspell
              (define-key evil-normal-state-map "]s" 'flyspell-goto-next-error))

            ;; fixup company-complete-number to be handled better with evil
            (evil-declare-change-repeat 'company-complete-number)))

(use-package evil-anzu
  :ensure t)

(use-package evil-args
  :ensure t
  :config (progn
            ;; bind evil-args text objects
            (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
            (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

            ;; bind evil-forward/backward-args
            (define-key evil-normal-state-map "L" 'evil-forward-arg)
            (define-key evil-normal-state-map "H" 'evil-backward-arg)
            (define-key evil-motion-state-map "L" 'evil-forward-arg)
            (define-key evil-motion-state-map "H" 'evil-backward-arg)))

(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :config (evil-commentary-mode t))

(use-package evil-leader
  :ensure t
  :config (progn
            (setq evil-leader/leader ","
                  evil-leader/in-all-states t)
            (evil-leader/set-key
              "c" 'avy-goto-char
              "fc" 'flycheck-buffer
              "fn" 'flycheck-next-error
              "fp" 'flycheck-previous-error
              "gc" 'ggtags-create-tags
              "gr" 'ggtags-find-reference
              "gs" 'ggtags-find-other-symbol
              "gt" 'ggtags-find-tag-regexp
              "gu" 'ggtags-update-tags
              "l" 'avy-goto-line
              "mg" 'magit-status
              "pa" 'helm-projectile-ag
              "pd" 'helm-projectile-find-file-dwim
              "pf" 'helm-projectile-find-file
              "po" 'helm-projectile-find-other-file
              "pp" 'helm-projectile-switch-project
              "sc" 'evil-surround-change
              "w" 'avy-goto-word-or-subword-1
              "x" 'helm-M-x
              "zf" 'vimish-fold-avy
              "SPC" 'evil-search-highlight-persist-remove-all))
  :init (global-evil-leader-mode 1))

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package evil-multiedit
  :ensure t
  :config (progn
            (bind-keys :map evil-visual-state-map
                       ;; Highlights all matches of the selection in the buffer.
                       ("R"   . evil-multiedit-match-all)
                       ;; Match selected region.
                       ("M-d" . evil-multiedit-match-and-next)
                       ;; Same as M-d but in reverse.
                       ("M-D" . evil-multiedit-match-and-prev))

            (bind-keys :map evil-normal-state-map
                       ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
                       ;; incrementally add the next unmatched match.
                       ("M-d" . evil-multiedit-match-and-next)
                       ;; Same as M-d but in reverse.
                       ("M-D" . evil-multiedit-match-and-prev))

            (bind-keys :map evil-multiedit-state-map
                       ;; For moving between edit regions
                       ("C-n" . evil-multiedit-next)
                       ("C-p" . evil-multiedit-prev))

            (bind-keys :map evil-multiedit-insert-state-map
                       ;; For moving between edit regions
                       ("C-n" . evil-multiedit-next)
                       ("C-p" . evil-multiedit-prev))

            ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
            (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)))

(use-package evil-numbers
  :ensure t
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-search-highlight-persist
  :ensure t
  :init (global-evil-search-highlight-persist t))

(use-package evil-space
  :ensure t
  :diminish evil-space-mode
  :init (evil-space-mode 1))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package evil-textobj-anyblock
  :ensure t
  :config (progn
            (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
            (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)))

(use-package evil-vimish-fold
  :ensure t)

(use-package evil-visualstar
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package fancy-battery
  :ensure t
  :defer t
  :config (fancy-battery-mode t))

(use-package fancy-narrow
  :ensure t
  :defer t
  :diminish fancy-narrow-mode
  :init (fancy-narrow-mode 1))

(use-package files
  :bind ("C-c r" . revert-buffer))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (unless (executable-find "shellcheck")
          (alert "shellcheck not found - is it installed? (shellcheck)"))
  :config (global-flycheck-mode +1))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :init (unless (executable-find "checkbashisms")
          (alert "checkbashisms not found - is it installed? (devscripts)"))
  :config (flycheck-checkbashisms-setup))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :config (progn
            (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
            (flycheck-add-next-checker 'irony '(warning . c/c++-cppcheck))))

;; we want to make sure irony comes before us in the list of flycheck-checkers
;; so do our cstyle setup after irony
(use-package flycheck-cstyle
  :ensure t
  :after flycheck-irony
  :init (unless (executable-find "cstyle")
          (alert "cstyle not found - is it install?"))
  :config (progn
            (add-hook 'flycheck-mode-hook #'flycheck-cstyle-setup)
            ;; chain after cppcheck so we have
            ;; irony->cppcheck->cstyle
            (flycheck-add-next-checker 'c/c++-cppcheck '(warning . cstyle))
            (unless (executable-find "cppcheck")
              (alert "cppcheck not found - is it installed?"))))

(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  :init (flycheck-package-setup))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :config (flycheck-pos-tip-mode))

(use-package flyspell
  :diminish flyspell-mode)

(use-package fuzzy
  :ensure t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package helm-fuzzier
  :ensure t
  :init (helm-fuzzier-mode t))

;; enable hlm-flx before helm
(use-package helm-flx
  :ensure t
  :init (helm-flx-mode t))

(use-package helm
  :ensure t
  :diminish helm-mode
  :defer t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf))
  :config (progn
            (require 'helm-config)
            ;; silence byte-compile warnings
            (eval-when-compile
              (require 'helm-command)
              (require 'helm-files))
            (setq helm-M-x-fuzzy-match t
                  helm-buffers-fuzzy-matching t
                  helm-recentf-fuzzy-match t)
            (helm-mode t)
            (helm-adaptive-mode t)
            ;; integrate with evil
            (with-eval-after-load 'evil
              (define-key evil-ex-map "b " 'helm-mini)
              (define-key evil-ex-map "e " 'helm-find-files)
              (evil-ex-define-cmd "ap[ropos]" 'helm-apropos)
              (define-key evil-ex-map "ap " 'helm-apropos))))

(use-package helm-ag
  :ensure t
  :config (progn
            ;; integrate with evil
            (with-eval-after-load 'evil
              (evil-ex-define-cmd "ag" 'helm-ag)
              (evil-ex-define-cmd "agi[nteractive]" 'helm-do-ag)
              (define-key evil-ex-map "ag " 'helm-ag)
              (define-key evil-ex-map "agi " 'helm-do-ag))))

(use-package helm-flyspell
  :ensure t
  :config (with-eval-after-load 'evil
            (define-key evil-normal-state-map "z=" 'helm-flyspell-correct)))

(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))

(use-package imenu
  :bind ("C-x C-i" . imenu))

(defun apm-irony-mode-setup ()
  "Setup irony-mode."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (irony-cdb-autosetup-compile-options)
  (with-eval-after-load 'company-irony
    (company-irony-setup-begin-commands))
  (with-eval-after-load 'irony-eldoc
    (irony-eldoc)))

(use-package irony
  :ensure t
  :diminish irony-mode
  :init (progn
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'irony-mode-hook 'apm-irony-mode-setup)))

(use-package irony-eldoc
  :ensure t)

(use-package ispell
  :defer t
  :config (when (executable-find "aspell")
            ;; use gb dictionary via aspell if available
            (setq ispell-program-name "aspell"
                  ispell-dictionary "british"
                  ispell-extra-args '("--sug-mode=ultra"))))

(use-package gdb-mi
  :config (progn
            ;; use gdb-many-windows by default
            (setq gdb-many-windows t)
            ;; Non-nil means display source file containing the main routine at startup
            (setq gdb-show-main t)))

(use-package gud
  :config (add-hook 'gud-mode-hook #'gud-tooltip-mode))

(defun apm-ggtags-setup ()
  "Setup ggtags for various modes."
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))

(use-package ggtags
  :ensure t
  :diminish (ggtags-mode ggtags-navigation-mode)
  :init (unless (executable-find "global")
          (alert "GNU Global not found - is it installed? - don't use Ubuntu package - too old!"))
  :config (progn
            (with-eval-after-load 'evil
              (evil-define-key 'normal ggtags-mode-map (kbd "C-]")
                #'ggtags-find-tag-dwim))
            (setq ggtags-enable-navigation-keys nil)
            ;; enable ggtags in all c common mode buffers
            (add-hook 'c-mode-common-hook #'apm-ggtags-setup)))

(use-package jenkins
  :ensure t
  ;; don't set jenkins-api-token here - do it in custom.el so it is not checked
  ;; into git
  :config (setq jenkins-hostname "http://cw-jenkins/jenkins/"
                jenkins-username "amurray"
                jenkins-viewname "RelX"))

(defun apm-js2-mode-setup ()
  "Setup js2-mode."
  (setq mode-name "js2"))

(use-package js2-mode
  :ensure t
  :defer t
  :config (progn
            (setq-default js2-basic-offset 2)

            (add-hook 'js2-mode-hook 'apm-js2-mode-setup)))

(defun apm-emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  (setq mode-name "el")
  ;; use aggressive indent
  (aggressive-indent-mode 1)
  (eldoc-mode t)
  ;; use smartparens in strict mode for lisp
  (with-eval-after-load 'smartparens
    (smartparens-strict-mode +1)))

(use-package lisp-mode
  :config (add-hook 'emacs-lisp-mode-hook #'apm-emacs-lisp-mode-setup))

;; full screen magit-status
(defun apm-fullscreen-magit-status (orig-fun &rest args)
  "Create a fullscreen `magit-status' via ORIG-FUN and ARGS."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun apm-quit-magit-session ()
  "Restore the previous window config and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :config (progn
            (advice-add 'magit-status :around #'apm-fullscreen-magit-status)
            (define-key magit-status-mode-map (kbd "q") 'apm-quit-magit-session)))

(use-package mallard-mode
  :ensure t
  :defer t)

(use-package mallard-snippets
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config (progn
            (unless (executable-find markdown-command)
              (alert "markdown not found - is it installed?"))))

(use-package paradox
  :ensure t
  ;; don't bother trying to integrate with github
  :config (setq paradox-github-token nil))

(use-package pdf-tools
  :ensure t
  :defer t
  :config (pdf-tools-install))

(use-package php-mode
  :ensure t
  :defer t)

(defun apm-prog-mode-setup ()
  "Tweaks and customisations for all programming modes."
  ;; highlight lines longer than 80 chars with column-enforce-mode
  (with-eval-after-load 'column-enforce-mode
    (column-enforce-mode))
  ;; turn on spell checking for strings and comments
  (flyspell-prog-mode)
  ;; highlight TODO and fixme so it looks scary
  (font-lock-add-keywords nil '(("\\<\\(TODO\\|ToDo\\|todo\\|FIXME\\|FixMe\\|fixme\\)" 1 font-lock-warning-face t))))

(use-package prog-mode
  :config (progn
            (when (boundp 'prettify-symbols-unprettify-at-point)
              ;; show original text when point is over a prettified symbol
              (setq prettify-symbols-unprettify-at-point 'right-edge))
            ;; prettify symbols (turn lambda -> λ)
            (global-prettify-symbols-mode 1)
            (add-hook 'prog-mode-hook #'apm-prog-mode-setup)))

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :bind ("C-x C-m" . projectile-compile-project)
  :init (progn
          (setq projectile-enable-caching t)
          (add-to-list 'projectile-project-root-files "configure.ac")
          (add-to-list 'projectile-project-root-files ".clang_complete")
          (add-to-list 'projectile-project-root-files ".clang_complete.in")
          (projectile-global-mode))
  :config (with-eval-after-load 'helm-projectile
            (setq projectile-completion-system 'helm)))

(use-package rainbow-mode
  :ensure t
  :init (dolist (hook '(css-mode-hook html-mode-hook))
          (add-hook hook #'rainbow-mode)))

(use-package region-state
  :ensure t
  :defer t
  :config (region-state-mode 1))

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1))

(use-package saveplace
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

(use-package scratch
  :ensure t)

(use-package semantic
  :defer t
  :config (progn
            ;; semantic and semanticdb - stores semantic information in a db so is
            ;; faster to compute next time a file is loaded
            (semantic-mode 1)
            (global-semanticdb-minor-mode 1)))

(use-package sh-mode
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

(use-package smart-mode-line
  :ensure t
  :defer t
  ;; seems we need to trust on first setup since custom file has not been read
  ;; yet when running emacs daemon
  :init (let ((sml/no-confirm-load-theme t))
          (sml/setup))
  :config (progn
            (add-to-list 'sml/replacer-regexp-list '("^~/dev/branches/RelX/" ":RelX:") t)
            (add-to-list 'sml/replacer-regexp-list '("^:RelX:\\(.*\\)/software/" ":RelX/\\1/:") t)
            (sml/apply-theme 'respectful)))

;; taken from https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312
(defun apm-c-mode-common-open-block (&rest ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent (IGNORED is ignored)."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init (smartparens-global-mode t)
  :config (progn
            (require 'smartparens-config)
            (setq sp-base-key-bindings 'paredit)
            ;; always jump out of string when hitting end "
            (setq sp-autoskip-closing-pair 'always)
            (setq sp-hybrid-kill-entire-symbol nil)
            (sp-use-paredit-bindings)

            ;; highlights matching pairs
            (show-smartparens-global-mode +1)

            ;; disable pairing of ' in minibuffer
            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

            ;; use smartparens to automatically indent correctly when opening
            ;; a new block
            (dolist (mode '(c-mode c++-mode java-mode))
              (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET"))))))

(use-package smooth-scrolling
  :ensure t
  :config (setq smooth-scroll-margin 5))

(use-package solarized-theme
  :ensure t
  :defer t
  :init (progn
          (setq x-underline-at-descent-line t)
          (setq solarized-distinct-fringe-background t)
          (load-theme 'solarized-light t)))

(use-package spaceline
  :ensure t
  :defer t
  :init (progn
          (require 'spaceline-config)
          ;; show evil state with colour change
          (setq spaceline-highlight-face-func
                #'spaceline-highlight-face-evil-state)
          (eval-when-compile
            (require 'spaceline-config))
          (spaceline-spacemacs-theme)))

(use-package tracwiki-mode
  :ensure t
  :defer t
  :commands tracwiki
  :config (tracwiki-define-project
           "mk2"
           "http://projects.cohda.wireless:8000/trac/mk2"))

(use-package type-break
  :defer t
  :config (type-break-mode))

(use-package unicode-fonts
  :ensure t
  :init (unicode-fonts-setup))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (progn
          (global-undo-tree-mode 1)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package vimish-fold
  :ensure t
  :config (vimish-fold-global-mode t))

(defun apm-web-mode-setup ()
  "Setup web mode."
  (setq mode-name "\uF0AC"))

(use-package web-mode
  :ensure t
  :commands web-mode
  :config (progn
            ;; use smartparens instead
            (setq web-mode-enable-auto-pairing nil)
            (add-hook 'web-mode-hook #'apm-web-mode-setup))
  :mode ("\\.php\\'" . web-mode))

(use-package which-func
  :config (which-function-mode t))

(use-package whitespace
  :diminish whitespace-mode)

(defun apm-insert-doxygen-function-snippet ()
  "Generate and expand a yasnippet template for function."
  (unless (and (fboundp 'semantic-current-tag)
               semantic-mode)
    (error "Semantic required to use dox snippet"))
  (let ((tag (senator-next-tag)))
    (while (or (null tag)
               (not (semantic-tag-of-class-p tag 'function)))
      (setq tag (senator-next-tag)))
    (let* ((name (semantic-tag-name tag))
           (attrs (semantic-tag-attributes tag))
           (args (plist-get attrs :arguments))
           (return-name (plist-get attrs :type))
           (idx 1))
      (if (listp return-name)
          (setq return-name (car return-name)))
      (yas-expand-snippet
       (format
        "/**
* @brief ${1:%s}
*
%s
%s*/
"
        name
        (mapconcat
         (lambda (x)
           (format "* @param %s ${%d:Description of %s}"
                   (car x) (incf idx) (car x)))
         args
         "\n")
        (if (and return-name (not (string-equal "void" return-name)))
            (format " * @return ${%d:%s}\n" (incf idx) return-name)
          ""))))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (progn
            ;; set this first so we don't get the bundled snippets loaded since
            ;; they don't generally match my desired style / indentation etc
            (setq yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
            (setq yas-prompt-functions '(yas-completing-prompt yas-no-prompt))
            (yas-global-mode t)))

(provide 'init)

;;; init.el ends here

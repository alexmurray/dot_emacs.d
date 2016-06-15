;;; init.el --- Starting point for Alex Murray's Emacs Configuration

;;; Commentary:
;;

;;; Code:

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;; uncomment to debug package loading times
;; (setq use-package-verbose t)

;; customisations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load custom but ignore error if doesn't exist
(load custom-file t)

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

;; automatically garbage collect when switch away from emacs
(add-hook 'focus-out-hook 'garbage-collect)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; set a reasonable fill and comment column
(setq-default fill-column 79)
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

;; ensure scrolling forwards / backwards preserves original location such that
;; they undo each other
(setq scroll-preserve-screen-position 'always)

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

;; Use regex searches and replace by default.
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-M-%" 'query-replace)

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
(bind-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)

;; from http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

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

;; autogenerate a .clang_complete if there is an associated .clang_complete.in
(defun apm-autogenerate-clang-complete ()
  "Autogenerate a .clang_complete if needed when opening a project."
  (when (and (fboundp 'projectile-project-root)
             ;; handle if not in project by returning nil
             (not (null (condition-case nil
                            (projectile-project-root)
                          (error nil))))
             (file-exists-p (concat (file-name-as-directory
                                     (projectile-project-root))
                                    ".clang_complete.in")))
    (projectile-with-default-dir (projectile-project-root)
      (shell-command "make .clang_complete"))))

(defun apm-after-find-file--auto-generate-clang-complete (&optional error warn noauto after-find-file-revert-buffer nomodes)
  "Try and autogenerate a .clang_complete (ERROR WARN NOAUTO AFTER-FIND-FILE-REVERT-BUFFER NOMODES are ignored)."
  (apm-autogenerate-clang-complete))

(advice-add 'after-find-file :before 'apm-after-find-file--auto-generate-clang-complete)

;;; Packages
(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package adaptive-wrap
  :ensure t)

(use-package abbrev
  :diminish abbrev-mode
  :config (progn
            (setq save-abbrevs t)
            (setq-default abbrev-mode t)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode)

(use-package ag
  :ensure t
  :defer t
  :init (unless (executable-find "ag")
          (alert "ag not found - is it installed?")))

(use-package android-mode
  :ensure t
  :commands (android-mode)
  :init (progn
          ;; change prefix so doesn't conflict with comment-region
          (setq android-mode-sdk-dir (expand-file-name "~/android-sdk-linux/")
                android-mode-key-prefix (kbd "C-c C-m"))
          (add-hook 'java-mode-hook #'android-mode))
  :diminish (android-mode . " \uf17b "))

(use-package anaconda-mode
  :ensure t
  ;; enable with apm-python-mode-setup below
  :defer t)

(use-package ansi-color
  ;; show colours correctly in shell
  :config (ansi-color-for-comint-mode-on))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(use-package apm-c
  :load-path "lisp/"
  :commands (apm-c-mode-setup)
  :init (dolist (hook '(c-mode-hook c++-mode-hook))
          (add-hook hook 'apm-c-mode-setup)))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package asn1-mode
  :ensure t
  :load-path "vendor/"
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :diminish auto-revert-mode
  :init (global-auto-revert-mode 1))

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
  ;; integrate with company
  (company-auctex-init))

(use-package auctex
  :ensure t
  :defer t
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
  :defer t
  :bind ("C-c SPC" . avy-goto-char)
  ;; dim text when avy is active
  :config (setq avy-background t))

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
  :defer t
  :init (add-hook 'c-mode-common-hook #'apm-c-mode-common-setup))

(use-package company
  :ensure t
  :commands global-company-mode
  :init (progn
          ;; set default lighter as nothing so in general it is not displayed
          ;; but will still be shown when completion popup is active to show the
          ;; backend which is in use
          (setq company-lighter-base "")
          (global-company-mode 1))
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
  :commands (company-anaconda)
  :after company
  :init (add-to-list 'company-backends #'company-anaconda))

(use-package company-auctex
  :ensure t
  ;; loaded in apm-latex-mode-setup
  :defer t)

(use-package company-dabbrev
  :after company
  ;; keep original case
  :config (setq company-dabbrev-downcase nil))

(use-package company-flx
  :ensure t
  :after company
  :init (company-flx-mode 1))

(use-package company-irony
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :after company
  :init (progn
          (setq company-irony-c-headers--compiler-executable
                (or (executable-find "clang++")
                    (executable-find "clang++-3.5")))
          ;; group with company-irony but beforehand so we get first pick
          (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

(use-package company-emoji
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-emoji))

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
  :defer t
  :init (add-hook 'company-mode-hook #'company-quickhelp-mode)
  :config (setq company-quickhelp-delay 0.1))

(use-package company-shell
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-shell))

(use-package company-statistics
  :ensure t
  :after company
  :config (company-statistics-mode 1))

(use-package company-try-hard
  :ensure t
  :after company
  :config (progn
            (global-set-key (kbd "C-<tab>") #'company-try-hard)
            (define-key company-active-map (kbd "C-<tab>") #'company-try-hard)))

(use-package company-web
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends 'company-web-html))

(use-package compile
  :bind ("C-x C-m" . compile)
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error))

(use-package counsel
  :ensure t
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-i" . counsel-imenu)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable))
  :init (progn
          (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
          (setq counsel-find-file-at-point t))
  :config (with-eval-after-load 'evil
            (evil-global-set-key 'normal [remap evil-search-forward] #'swiper)
            (evil-global-set-key 'normal [remap evil-search-backward] #'swiper)
            (evil-global-set-key 'motion [remap evil-search-forward] #'swiper)
            (evil-global-set-key 'motion [remap evil-search-backward] #'swiper)))

(use-package counsel-projectile
  :ensure t
  ;; defer since we bind with evil-leader
  :defer t)

(defun apm-coverlay-setup()
  (coverlay-mode 1))

(use-package coverlay
  :ensure t
  :defer t
  :diminish coverlay-mode
  :config (add-hook 'c-mode-common-hook #'apm-coverlay-setup))

(use-package cstyle
  :load-path "vendor/cstyle.el")

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode 1))

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
          (global-diff-hl-mode 1)
          ;; highlight in unsaved buffers as well
          (diff-hl-flydiff-mode 1)
          ;; Integrate with Magit
          (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
          ;; Highlight changed files in the fringe of dired
          (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

(use-package diminish
  :ensure t)

(defun apm-doxymacs-setup()
  (doxymacs-mode)
  (doxymacs-font-lock))

(use-package doxymacs
  :defer t
  :commands (doxymacs-mode doxymacs-font-lock)
  :diminish doxymacs-mode
  :config (add-hook 'c-mode-common-hook #'apm-doxymacs-setup))

(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

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
  :commands (eldoc-mode)
  ;; enable eldoc in eval-expression
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package electric
  :init (progn
          ;; electric indent and layout modes to make more IDE like
          (electric-indent-mode 1)
          (electric-layout-mode 1)))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook #'elisp-slime-nav-mode))
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

            (dolist (mode '(bs-mode
                            comint-mode
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

            ;; these should be bound automatically but apparently not so rebind
            ;; them
            (bind-keys :map evil-insert-state-map
                       ("C-x C-n" . evil-complete-next-line)
                       ("C-x C-p" . evil-complete-previous-line))

            ;; fixup company-complete-number to be handled better with evil
            (evil-declare-change-repeat 'company-complete-number)
            (evil-mode 1)))

(use-package evil-anzu
  :ensure t)

(use-package evil-args
  :ensure t
  :defer t
  :init (progn
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
  :config (evil-commentary-mode 1))

(use-package evil-leader
  :ensure t
  :config (progn
            (setq evil-leader/leader ","
                  evil-leader/in-all-states t)
            (evil-leader/set-key
              "," 'counsel-projectile
              "ac" 'avy-goto-char
              "al" 'avy-goto-line
              "aw" 'avy-goto-word-or-subword-1
              "ag" 'counsel-ag
              "b" 'ivy-switch-buffer
              "cr" 'comment-or-uncomment-region
              "fc" 'flycheck-buffer
              "fn" 'flycheck-next-error
              "fp" 'flycheck-previous-error
              "gc" 'ggtags-create-tags
              "gd" 'ggtags-delete-tags
              "ge" 'google-error
              "gg" 'counsel-git-grep
              "go" 'google-this
              "gr" 'ggtags-find-reference
              "gs" 'ggtags-find-other-symbol
              "gt" 'ggtags-find-tag-regexp
              "gu" 'ggtags-update-tags
              "mg" 'magit-status
              "pa" 'projectile-ag
              "pe" 'projectile-switch-to-eshell
              "pd" 'projectile-find-file-dwim
              "pf" 'counsel-projectile-find-file
              "po" 'projectile-find-other-file
              "pp" 'projectile-switch-project
              "sc" 'evil-surround-change
              "x" 'counsel-M-x
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
  :init (global-evil-search-highlight-persist 1))

(use-package evil-smartparens
  :ensure t
  :defer t
  :diminish evil-smartparens-mode
  ;; only use with strict smartparens otherwise is too annoying for normal cases
  :init (add-hook 'smartparens-strict-mode-hook #'evil-smartparens-mode))

(use-package evil-space
  :ensure t
  :diminish evil-space-mode
  :init (evil-space-mode 1))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package evil-textobj-anyblock
  :ensure t
  :bind ((:map evil-inner-text-objects-map ("b" . 'evil-textobj-anyblock-inner-block)
               :map evil-outer-text-objects-map ("b" . 'evil-textobj-anyblock-a-block))))

(use-package evil-vimish-fold
  :ensure t
  :diminish evil-vimish-fold-mode
  :config (evil-vimish-fold-mode 1))

(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package eyebrowse
  :ensure t
  :after evil
  :config (progn
            (eyebrowse-mode t)
            ;; start a new workspace clean with just the scratch buffer
            (setq eyebrowse-new-workspace t)
            ;; wrap workspaces like vim
            (setq eyebrowse-wrap-around t)
            (eyebrowse-setup-evil-keys)))

(use-package fancy-battery
  :ensure t
  :config (fancy-battery-mode 1))

(use-package fancy-narrow
  :ensure t
  :diminish fancy-narrow-mode
  :config (fancy-narrow-mode 1))

(use-package files
  :bind ("C-c r" . revert-buffer))

(use-package fill-column-indicator
  :ensure t
  :config (progn
            (define-global-minor-mode global-fci-mode fci-mode
              ;; only enable when buffer is not a special buffer (starts and
              ;; ends with an asterisk)
              (lambda () (if (not (string-match "^\*.*\*$" (buffer-name)))
                             (fci-mode 1))))
            (global-fci-mode 1)
            ;; make fci play nicely with company-mode - from https://github.com/alpaker/Fill-Column-Indicator/issues/54#issuecomment-218344694
            (with-eval-after-load 'company
              (defun on-off-fci-before-company(command)
                (when (string= "show" command)
                  (turn-off-fci-mode))
                (when (string= "hide" command)
                  (turn-on-fci-mode)))

              (advice-add 'company-call-frontends :before #'on-off-fci-before-company))))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (unless (executable-find "shellcheck")
          (alert "shellcheck not found - is it installed? (shellcheck)"))
  :config (global-flycheck-mode 1))

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
  :config (flycheck-pos-tip-mode 1))

(use-package flyspell
  :diminish flyspell-mode)

(use-package flyspell-correct-ivy
  :ensure t
  :after ivy
  ;; use instead of ispell-word which evil binds to z=
  :config (bind-key [remap ispell-word] 'flyspell-correct-word-generic))

(use-package flx
  :ensure t)

(use-package fuzzy
  :ensure t)

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package google-this
  :ensure t
  :commands (google-this google-error))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :commands (ivy-mode ivy-read ivy-resume)
  :bind (("C-c C-r" . ivy-resume))
  :init (progn
          ;; show recent files and bookmarks with `ivy-switch-buffer'
          (setq ivy-use-virtual-buffers t)
          (setq ivy-display-style 'fancy)
          (bind-key [remap switch-to-buffer] 'ivy-switch-buffer)
          (ivy-mode 1))
  :config (progn
            ;; restore behaviour of C-s C-w to search for word at point
            (define-key ivy-minibuffer-map (kbd "C-w") 'ivy-yank-word)))

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
  :commands (irony-mode)
  :init (progn
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'irony-mode-hook 'apm-irony-mode-setup)))

(use-package irony-eldoc
  :ensure t
  :defer t)

(use-package ispell
  :defer t
  :init (when (executable-find "aspell")
          ;; use gb dictionary via aspell if available
          (setq ispell-program-name "aspell"
                ispell-dictionary "british"
                ispell-extra-args '("--sug-mode=ultra"))))

(use-package gdb-mi
  :defer t
  :init (progn
          ;; use gdb-many-windows by default
          (setq gdb-many-windows t)
          ;; Non-nil means display source file containing the main routine at startup
          (setq gdb-show-main t)))

(use-package gud
  :defer t
  :init (add-hook 'gud-mode-hook #'gud-tooltip-mode))

(defun apm-ggtags-setup ()
  "Setup ggtags for various modes."
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index)))

(use-package ggtags
  :ensure t
  :defer t
  :diminish (ggtags-mode ggtags-navigation-mode)
  :init (progn
          (unless (executable-find "global")
            (alert "GNU Global not found - is it installed? - don't use Ubuntu package - too old!"))
          (with-eval-after-load 'evil
            (evil-define-key 'visual ggtags-mode-map (kbd "C-]")
              #'ggtags-find-tag-dwim)
            (evil-define-key 'normal ggtags-mode-map (kbd "C-]")
              #'ggtags-find-tag-dwim))
          (setq ggtags-enable-navigation-keys nil)
          ;; enable ggtags in all c common mode buffers
          (add-hook 'c-mode-common-hook #'apm-ggtags-setup)))

(use-package jenkins
  :ensure t
  :commands (jenkins)
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
  :init (progn
          (setq-default js2-basic-offset 2)
          (add-hook 'js2-mode-hook 'apm-js2-mode-setup)))

(defun apm-emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  (setq mode-name "el")
  ;; use aggressive indent
  (aggressive-indent-mode 1)
  (eldoc-mode 1)
  ;; use smartparens in strict mode for lisp
  (with-eval-after-load 'smartparens
    (smartparens-strict-mode 1)))

(use-package lisp-mode
  :config (add-hook 'emacs-lisp-mode-hook #'apm-emacs-lisp-mode-setup))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package mallard-mode
  :ensure t
  :defer t)

(use-package mallard-snippets
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :config (progn
            (unless (executable-find markdown-command)
              (alert "markdown not found - is it installed?"))))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  ;; don't bother trying to integrate with github
  :init (setq paradox-github-token nil))

(use-package pdf-tools
  :ensure t
  :defer 5
  :config (pdf-tools-install))

(defun apm-prog-mode-setup ()
  "Tweaks and customisations for all programming modes."
  ;; turn on spell checking for strings and comments
  (flyspell-prog-mode)
  ;; use drag stuff
  (drag-stuff-mode 1)
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
  :bind (("C-x C-m" . projectile-compile-project)
         ("C-x C-g" . projectile-find-file))
  :init (progn
          (setq projectile-enable-caching t)
          (projectile-global-mode))
  :config (progn
            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".clang_complete")
            (add-to-list 'projectile-project-root-files ".clang_complete.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")
            (with-eval-after-load 'ivy
              (setq projectile-completion-system 'ivy))))

(defun apm-python-mode-setup ()
  "Tweaks and customisations for `python-mode'."
  (setq python-indent-offset 4)
  (anaconda-mode 1)
  (anaconda-eldoc-mode 1))

(use-package python
  :defer t
  :init (add-hook 'python-mode-hook #'apm-python-mode-setup))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init (dolist (hook '(css-mode-hook html-mode-hook))
          (add-hook hook #'rainbow-mode)))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1))

(use-package saveplace
  :config (progn
            (setq-default save-place t)
            (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

(use-package scratch
  :ensure t
  :defer t)

(use-package sh-script
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

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
  :init (smartparens-global-mode 1)
  :config (progn
            (require 'smartparens-config)
            (setq sp-base-key-bindings 'paredit)
            ;; always jump out of string when hitting end "
            (setq sp-autoskip-closing-pair 'always)
            (setq sp-hybrid-kill-entire-symbol nil)
            (sp-use-paredit-bindings)

            ;; highlights matching pairs
            (show-smartparens-global-mode 1)

            ;; disable pairing of ' in minibuffer
            (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

            ;; use smartparens to automatically indent correctly when opening
            ;; a new block
            (dolist (mode '(c-mode c++-mode java-mode))
              (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET"))))))

(use-package smooth-scrolling
  :ensure t
  :config (progn
            (setq smooth-scroll-margin 2)
            (smooth-scrolling-mode 1)))

(use-package solarized-theme
  :ensure t
  :config (progn
            (setq x-underline-at-descent-line t)
            (setq solarized-distinct-fringe-background t)
            (load-theme 'solarized-dark t)))

(use-package spaceline-config
  :ensure spaceline
  :init (setq spaceline-workspace-numbers-unicode t
              spaceline-window-numbers-unicode t)
  :config (progn
            (require 'spaceline-config)
            ;; show evil state with colour change
            (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
            (spaceline-spacemacs-theme)))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package tracwiki-mode
  :ensure t
  :defer t
  :commands tracwiki
  :config (tracwiki-define-project
           "mk2"
           "http://projects.cohda.wireless:8000/trac/mk2"))

(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package vimish-fold
  :ensure t
  :config (vimish-fold-global-mode 1))

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
  :config (which-function-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace
  :diminish whitespace-mode)

(defun apm-insert-doxygen-function-snippet ()
  "Generate and expand a yasnippet template for function."
  (unless (or semantic-mode (require 'semantic nil t))
    (error "Semantic required to use dox snippet"))
  (let ((semantic-enabled semantic-mode))
    (unless semantic-enabled
      (semantic-mode 1)
      (semantic-fetch-tags))
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
            "")))))
    (unless semantic-enabled
      (semantic-mode 0))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode 1))

(provide 'init)

;;; init.el ends here

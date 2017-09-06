;;; init.el --- Starting point for Alex Murray's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)
;; fix recursive load *.el.gz issue with emacs-snapshot -
;; https://github.com/purcell/emacs.d/issues/340#issuecomment-237177032
(setq load-file-rep-suffixes '(""))
;; uncomment to debug package loading times
;; (setq use-package-verbose t)

;; customisations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load custom but ignore error if doesn't exist
(load custom-file t)

;; Linux package management
(require 'dbus)

(defun pk-install-package (name)
  "Install a package with NAME using PackageKit."
  (interactive "sPackage to install: ")
  (condition-case ex
      (let ((xid (cdr (assoc 'outer-window-id (frame-parameters)))))
        (dbus-call-method :session
                          "org.freedesktop.PackageKit"
                          "/org/freedesktop/PackageKit"
                          "org.freedesktop.PackageKit.Modify"
                          "InstallPackageNames"
                          (if xid xid 0)
                          `(:array ,name)
                          "show-confirm-search,hide-finished"))
    (error (format "Error trying to install package %s: %s" name ex))))

;;; Package management
(require 'package)
;; we use use-package to do this for us
(setq package-enable-at-startup nil)
;; use https for both melpa and gelpa
(eval-and-compile
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org" . "http://orgmode.org/elpa/"))))

(require 'tls)
(require 'gnutls)
;; ensure certificate validation is setup - https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(let ((trustfile (replace-regexp-in-string
                  (rx (* (any " \t\n")) eos)
                  ""
                  (shell-command-to-string "python -m certifi"))))
  (unless (file-exists-p trustfile)
    (unless (executable-find "pip")
      (pk-install-package "python-pip"))
    (call-process "pip" nil nil nil "install" "--user" "certifi")
    (setq trustfile (replace-regexp-in-string
                     (rx (* (any " \t\n")) eos)
                     ""
                     (shell-command-to-string "python -m certifi"))))
  (unless (executable-find "gnutls-cli")
    (pk-install-package "gnutls-bin"))
  (setq tls-program (list (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile))
        gnutls-verify-error t
        gnutls-trustfiles (list trustfile)))

(package-initialize)

;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package alert
  :ensure t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

;; some useful functions for the rest of this init file
(defun apm-camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(defvar apm-preferred-emacs-version "25.2")
(when (version< emacs-version apm-preferred-emacs-version)
  (alert (format "Emacs version too old - please run %s or newer"
                 apm-preferred-emacs-version)
         :severity 'high))

;;; General settings etc

;; automatically garbage collect when switch away from emacs
(add-hook 'focus-out-hook 'garbage-collect)

;; set high gc limit for minibuffer so doesn't slowdown on helm etc
(defun apm-minibuffer-setup ()
  "Setup minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun apm-minibuffer-exit ()
  "Undo minibuffer setup."
  (setq gc-cons-threshold (* 64 1024 1024)))

(add-hook 'minibuffer-setup-hook #'apm-minibuffer-setup)
(add-hook 'minibuffer-exit-hook #'apm-minibuffer-exit)

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; tabs are never ok
(setq-default indent-tabs-mode nil)

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
;; don't use gtk style tooltips since are intrusive
(setq-default x-gtk-use-system-tooltips nil)
(blink-cursor-mode -1)
(setq frame-title-format
      '((buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(setq icon-title-format frame-title-format)

;; default to maximised windows
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; prompt when trying to switch out of a dedicated window
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; ensure scrolling forwards / backwards preserves original location such that
;; they undo each other
(setq scroll-preserve-screen-position 'always)

(defvar apm-preferred-font-family "Inconsolata"
  "Preferred font family to use.")

(defvar apm-preferred-font-family-package "fonts-inconsolata"
  "Package to install to get `apm-preferred-font-family'.")

(defvar apm-preferred-font-height 117
  "Preferred font height to use.")

(defun apm-graphic-frame-init (&optional frame)
  "Initialise properties specific to graphical display for FRAME."
  (interactive)
  (when (display-graphic-p)
    (if (font-info apm-preferred-font-family)
        (set-face-attribute 'default frame
                            :family apm-preferred-font-family
                            :height apm-preferred-font-height)
      (pk-install-package apm-preferred-font-family-package))))

;; make sure graphical properties get set on client frames
(add-hook 'after-make-frame-functions #'apm-graphic-frame-init)
(add-hook 'server-visit-hook #'apm-graphic-frame-init)
(apm-graphic-frame-init)

;; Use regex searches and replace by default.
(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)
(bind-key "C-M-%" 'query-replace)

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

;;; Packages
(use-package abbrev
  :diminish abbrev-mode
  :config (progn
            (setq save-abbrevs t)
            (setq-default abbrev-mode t)))

(use-package ace-window
  :ensure t
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :init (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :init (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package ag
  :ensure t
  :defer t
  :init (unless (executable-find "ag")
          (pk-install-package "silversearcher-ag")))

(use-package all-the-icons
  :ensure t
  :config (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
            (all-the-icons-install-fonts)))

(use-package android-mode
  :ensure t
  :defer t
  :commands (android-mode)
  :init (progn
          ;; change prefix so doesn't conflict with comment-region
          (setq android-mode-sdk-dir (expand-file-name "~/android-sdk-linux/")
                android-mode-key-prefix (kbd "C-c C-m"))
          (add-hook 'java-mode-hook #'android-mode))
  :diminish (android-mode . "  "))

(use-package anaconda-mode
  :ensure t
  :diminish (anaconda-mode . " 🐍 ")
  :defer t
  :init (progn
          (add-hook 'python-mode-hook #'anaconda-mode)
          (add-hook 'python-mode-hook #'anaconda-eldoc-mode)))

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
  :load-path "lisp/apm-c")

(defun apm-appt-notify (time-to-appt time msg)
  "Notify for appointment at TIME-TO-APPT TIME MSG alert."
  (alert msg
         :title (format "Appointment in %s minutes [%s]" time-to-appt time)
         :icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png"))

(use-package appt
  :config (progn
            (setq appt-disp-window-function #'apm-appt-notify)
            (appt-activate 1)))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package arxml-mode
  :load-path "vendor/"
  :demand t)

(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :diminish auto-revert-mode
  :init (global-auto-revert-mode 1))

(defun apm-latex-mode-setup ()
  "Tweaks and customisations for LaTeX mode."
  ;; Enable source-correlate for Control-click forward/reverse search.
  (TeX-source-correlate-mode 1)
  ;; enable math mode in latex
  (LaTeX-math-mode 1)
  ;; Enable reftex
  (turn-on-reftex))

(use-package auctex
  :ensure t
  :defer t
  :commands (LaTeX-math-mode TeX-source-correlate-mode)
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
  ;; dim text when avy is active
  :config (setq avy-background t))

(use-package beginend
  :ensure t
  :commands (beginend-global-mode beginend-prog-mode)
  :diminish (beginend-global-mode beginend-prog-mode)
  :config (beginend-global-mode 1))

(use-package browse-kill-ring
  :ensure t)

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package bug-reference
  :defer t
  :init (progn
          (eval-when-compile
            (require 'bug-reference))
          (setq bug-reference-url-format "http://projects.cohda.wireless:8000/trac/mk2/ticket/%s"
                bug-reference-bug-regexp "\\([Tt]icket ?#?:?\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)")
          (add-hook 'prog-mode-hook #'bug-reference-prog-mode)))

(use-package cargo
  :ensure t
  :defer t
  :init (add-hook 'rust-mode-hook 'cargo-minor-mode))

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
  (c-toggle-auto-hungry-state 1)
  ;; turn on electric indent
  (c-toggle-electric-state 1)
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
  :commands (c-toggle-auto-hungry-state)
  :init (add-hook 'c-mode-common-hook #'apm-c-mode-common-setup))

(defun apm-c-mode-setup ()
  "Tweaks and customisations for `c-mode'."
  (c-set-style "cohda")
  ;; and treat linux style as safe for local variable
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux))
  ;; ensure fill-paragraph takes doxygen @ markers as start of new
  ;; paragraphs properly
  (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f")
  (with-eval-after-load 'helm-dash
    (eval-when-compile
      (defvar helm-dash-docsets nil))
    (setq-local helm-dash-docsets '("C" "C++" "GLib"))))

(use-package cohda-c
  :load-path "lisp/"
  :init (dolist (hook '(c-mode-hook c++-mode-hook))
          (add-hook hook 'apm-c-mode-setup)))

(use-package cmake-mode
  :ensure t
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config (unless (executable-find "cmake")
            (pk-install-package "cmake")))

(use-package company
  :ensure t
  :commands global-company-mode
  ;; Use Company for completion
  :bind (("C-;" . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol] . company-complete-common))
  :init (progn
          ;; set default lighter as nothing so in general it is not displayed
          ;; but will still be shown when completion popup is active to show the
          ;; backend which is in use
          (setq company-lighter-base "")
          (global-company-mode 1))
  :config (progn
            ;; ensure flyspell doesn't steal our binding of C-;
            (eval-when-compile
              (require 'flyspell))
            (with-eval-after-load 'flyspell
              (bind-key "C-;" nil flyspell-mode-map))
            ;; some better default values
            (setq company-idle-delay 0.2)
            (setq company-tooltip-limit 10)
            (setq company-minimum-prefix-length 1)
            (setq company-selection-wrap-around t)

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
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'company-auctex-init))

(use-package company-dabbrev
  :after company
  ;; keep original case
  :config (setq company-dabbrev-downcase nil))

(use-package company-flx
  :ensure t
  :disabled t
  :after company
  :init (company-flx-mode 1))

(use-package company-irony
  :ensure t
  :after company
  :init (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :after company
  :config (progn
            (setq company-irony-c-headers--compiler-executable (executable-find "clang++"))
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

(use-package company-statistics
  :ensure t
  :after company
  :config (company-statistics-mode 1))

(use-package company-tracwiki
  :load-path "vendor/"
  :defer t
  :after tracwiki-mode
  :init (add-to-list 'company-backends 'company-tracwiki))

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
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error))

(use-package cov
  :ensure t
  :defer t
  :diminish cov-mode
  :init (add-hook 'c-mode-common-hook #'cov-mode))

(use-package crux
  :ensure t
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ("C-c o" . crux-open-with)))

(use-package cstyle
  :load-path "vendor")

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode 1))

;; show suspicious c constructs automatically
(use-package cwarn
  :diminish cwarn-mode
  :init (global-cwarn-mode 1))

(use-package dbc-mode
  :load-path "vendor/")

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(use-package diff
  ;; default to unified diff
  :config (setq diff-switches "-u"))

(use-package diff-hl
  :ensure t
  :init (progn
          (global-diff-hl-mode 1)
          ;; don't highlight in unsaved buffers since slows down too much -
          ;; especially when using spaceline-all-the-icons - no real need since
          ;; diff info is most useful on save
          (diff-hl-flydiff-mode -1)
          ;; Integrate with Magit
          (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
          ;; Highlight changed files in the fringe of dired
          (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

(use-package diminish
  :ensure t)

(use-package doxyas
  :load-path "vendor/"
  :commands doxyas-document-function
  ;; defer since is bound via evil-leader
  :defer t)

(defun apm-doxymacs-setup()
  "Setup doxymacs."
  (when (require 'doxymacs nil t)
    (doxymacs-mode)
    (doxymacs-font-lock)))

(use-package doxymacs
  :defer t
  :commands (doxymacs-mode doxymacs-font-lock)
  :diminish doxymacs-mode
  :init (progn
          (unless (require 'doxymacs nil t)
            (alert "doxymacs not found - is it installed? (don't use Ubuntu package since requires Emacs 24)"))
          (add-hook 'c-mode-common-hook #'apm-doxymacs-setup)))

(use-package drag-stuff
  :ensure t
  :defer t
  :diminish drag-stuff-mode
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down))
  :init (add-hook 'prog-mode-hook #'drag-stuff-mode))

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
  :config (global-eldoc-mode 1))

(use-package elf-mode
  :ensure t)

(use-package electric
  :init (progn
          ;; electric indent and layout modes to make more IDE like
          (electric-indent-mode 1)
          (electric-layout-mode 1)))

(use-package elisp-slime-nav
  :ensure t
  :defer t
  :after evil
  :diminish elisp-slime-nav-mode
  :init (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
          (add-hook hook #'elisp-slime-nav-mode))
  :config (progn
            (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "C-]")
              #'elisp-slime-nav-find-elisp-thing-at-point)
            (evil-define-key 'visual elisp-slime-nav-mode-map (kbd "C-]")
              #'elisp-slime-nav-find-elisp-thing-at-point)
            (evil-define-key 'normal elisp-slime-nav-mode-map (kbd "M-*")
              #'pop-tag-mark)))

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


(use-package eshell
  :defer t
  :commands eshell
  :bind ("C-x m" . eshell))

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

            ;; TODO: move these into their own mode-specific sections since
            ;; evil shouldn't have to know about every other mode...
            (dolist (mode '(bs-mode
                            comint-mode
                            edebug-mode
                            eshell-mode
                            git-commit-mode
                            git-rebase-mode
                            jenkins-mode
                            jenkins-job-view-mode
                            inferior-emacs-lisp-mode
                            magit-branch-manager-mode
                            magit-popup-mode
                            magit-popup-sequence-mode
                            magit-blame-mode
                            paradox-menu-mode
                            pcap-mode
                            pylookup-mode
                            semantic-symref-results-mode
                            shell-mode
                            svn-status-mode
                            term-mode))
              (evil-set-initial-state mode 'emacs))

            ;; add vim-like bindings for some nice stuff
            (with-eval-after-load 'flyspell
              (define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
              ;; taken from spacemacs
              (define-key evil-normal-state-map "[b" 'evil-prev-buffer)
              (define-key evil-normal-state-map "]b" 'evil-next-buffer)
              (define-key evil-normal-state-map "[q" 'previous-error)
              (define-key evil-normal-state-map "]q" 'next-error))

            ;; these should be bound automatically but apparently not so rebind
            ;; them
            (bind-keys :map evil-insert-state-map
                       ("C-x C-n" . evil-complete-next-line)
                       ("C-x C-l" . evil-complete-next-line)
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

(use-package evil-goggles
  :ensure t
  :diminish evil-goggles-mode
  :config (progn
            (setq evil-goggles-duration 0.1)
            (evil-goggles-mode 1)
            (evil-goggles-use-diff-faces)))

(use-package evil-leader
  :ensure t
  :config (progn
            (setq evil-leader/leader "<SPC>"
                  evil-leader/in-all-states t)
            (evil-leader/set-key
              "SPC" 'avy-goto-word-or-subword-1
              "l" 'avy-goto-line
              "c" 'avy-goto-char-timer
              "a" 'helm-ag
              "b" 'helm-mini
              "dd" 'helm-dash-at-point
              "df" 'doxyas-document-function
              "e" 'eshell
              "fc" 'flycheck-buffer
              "ff" 'helm-find-files
              "ge" 'google-error
              "gg" 'helm-grep-do-git-grep
              "go" 'google-this
              "gc" 'ggtags-create-tags
              "gd" 'ggtags-delete-tags
              "gr" 'ggtags-find-reference
              "gs" 'ggtags-find-tag-regexp
              "gt" 'ggtags-find-definition
              "gu" 'ggtags-update-tags
              "hd" 'helm-dash
              "i" 'helm-semantic-or-imenu
              "mg" 'magit-status
              "mm" 'magit-dispatch-popup
              "ms" 'svn-status
              "oa" 'org-agenda
              "ob" 'org-ido-switchb
              "oca" 'org-capture
              "occ" 'org-clock-cancel
              "ocd" 'org-clock-display
              "ocg" 'org-clock-goto
              "oci" 'org-clock-in
              "oco" 'org-clock-out
              "ocs" 'org-mru-clock-in
              "ocu" 'org-clock-update-time-maybe
              "oo" 'helm-org-agenda-files-headings
              "ot" 'org-todo-list
              "pa" 'helm-projectile-ag
              "pb" 'helm-projectile-switch-to-buffer
              "pe" 'projectile-run-eshell
              "pd" 'helm-projectile-find-dir
              "pf" 'helm-projectile-find-file
              "pg" 'helm-projectile-grep
              "ph" 'helm-projectile
              "pk" 'projectile-kill-buffers
              "po" 'helm-projectile-find-other-file
              "pp" 'helm-projectile-switch-project
              "pr" 'helm-projectile-recentf
              "r" 'helm-recentf
              "s" 'helm-swoop
              "u" 'helm-ucs
              "v" 'er/expand-region
              "x" 'helm-M-x))
  :init (global-evil-leader-mode 1))

(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package evil-numbers
  :ensure t
  :bind (("C-c +" . evil-numbers/inc-at-pt)
         ("C-c -" . evil-numbers/dec-at-pt)))

(use-package evil-smartparens
  :ensure t
  :defer t
  :diminish evil-smartparens-mode
  ;; only use with strict smartparens otherwise is too annoying for normal cases
  :init (add-hook 'smartparens-strict-mode-hook #'evil-smartparens-mode))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode 1))

(use-package evil-textobj-anyblock
  :ensure t
  :bind ((:map evil-inner-text-objects-map ("b" . 'evil-textobj-anyblock-inner-block)
               :map evil-outer-text-objects-map ("b" . 'evil-textobj-anyblock-a-block))))

(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package expand-region
  :ensure t
  :config (setq expand-region-contract-fast-key "V"
                expand-region-reset-fast-key "r"))

(use-package fancy-battery
  :ensure t
  :config (fancy-battery-mode 1))

(use-package fic-mode
  :ensure t
  :defer t
  :functions fic-mode
  :init (add-hook 'prog-mode-hook #'fic-mode))

(use-package files
  :bind ("C-c r" . revert-buffer))

(defun apm-flycheck-setup ()
  "Setup flycheck."
  (define-key evil-normal-state-map "[e" 'flycheck-previous-error)
  (define-key evil-normal-state-map "]e" 'flycheck-next-error))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands flycheck-add-next-checker
  :init (progn
          (setq-default flycheck-emacs-lisp-load-path 'inherit)
          (unless (executable-find "shellcheck")
            (pk-install-package "shellcheck"))
          (unless (executable-find "cppcheck")
            (pk-install-package "cppcheck"))
          (add-hook 'flycheck-mode-hook #'apm-flycheck-setup))
  :config (progn
            ;; Ubuntu 16.04 shellcheck is too old to understand this
            ;; command-line option
            (setq flycheck-shellcheck-follow-sources nil)
            (global-flycheck-mode 1)))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :init (unless (executable-find "checkbashisms")
          (pk-install-package "devscripts"))
  :config (flycheck-checkbashisms-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :commands flycheck-clang-analyzer-setup
  :after flycheck-cstyle
  :init (unless (executable-find "clang-4.0")
          (pk-install-package "clang-4.0"))
  :config (progn
            (setq flycheck-clang-analyzer-executable "clang-4.0")
            (flycheck-clang-analyzer-setup)
            ;; automatically sets itself up as next checker after irony so undo
            ;; that
            (delete '(warning . clang-analyzer)
                    (flycheck-checker-get 'irony 'next-checkers))
            (flycheck-add-next-checker 'c/c++-cppcheck '(t . clang-analyzer))))

(use-package flycheck-coverity
  :ensure t
  :commands flycheck-coverity-setup
  :after flycheck-clang-analyzer
  :init (unless (executable-find "cov-run-desktop")
          (alert "cov-run-desktop not found - is it installed?"))
  :config (progn
            (flycheck-coverity-setup)
            (flycheck-add-next-checker 'clang-analyzer '(t . coverity))))

;; we want to make sure clang-analyzer comes before us in the list of flycheck-checkers
;; so do our cstyle setup after clang-analyzer
(use-package flycheck-cstyle
  :ensure t
  :after flycheck-irony
  :init (unless (executable-find "cstyle")
          (alert "cstyle not found - is it installed?"))
  :config (progn
            (flycheck-cstyle-setup)
            (flycheck-add-next-checker 'irony '(warning . cstyle))
            (flycheck-add-next-checker 'cstyle '(t . c/c++-cppcheck))))

(use-package flycheck-flawfinder
  :ensure t
  :disabled t
  :after flycheck-cstyle
  :init (unless (executable-find "flawfinder")
          (pk-install-package "flawfinder"))
  :config (progn
            (flycheck-flawfinder-setup)
            (flycheck-add-next-checker 'irony '(warning . flawfinder) t)))

(use-package flycheck-irony
  :ensure t
  :after flycheck
  :config (flycheck-irony-setup))

(use-package flycheck-jing
  :load-path "vendor/"
  :after flycheck
  :config (flycheck-jing-setup))

(use-package flycheck-package
  :ensure t
  :defer t
  :after flycheck
  :config (flycheck-package-setup))

(use-package flycheck-plantuml
  :ensure t
  :after flycheck
  :config (flycheck-plantuml-setup))

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :config (progn
            (setq flycheck-popup-tip-error-prefix "")
            (flycheck-popup-tip-mode 1)))

(use-package flycheck-rust
  :ensure t
  :config (flycheck-rust-setup))

(use-package flyspell
  :diminish flyspell-mode
  :defer t
  :init (progn
          (add-hook 'text-mode-hook #'flyspell-mode)
          (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package flx
  :ensure t)

(use-package fuzzy
  :ensure t)

(use-package gdb-mi
  :defer t
  :init (progn
          ;; use gdb-many-windows by default
          (setq-default gdb-many-windows t)
          ;; Non-nil means display source file containing the main routine at startup
          (setq-default gdb-show-main t)))

(defun apm-ggtags-setup ()
  "Setup ggtags for various modes."
  (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
    (ggtags-mode 1)
    ;; disable navigation since conflicts with usual keybindings
    (ggtags-navigation-mode -1)))

(use-package ggtags
  :ensure t
  :diminish ggtags-mode
  :init (progn
          (unless (executable-find "global")
            (alert "GNU Global not found - use ppa:alexmurray/global"))
          (with-eval-after-load 'evil
            (evil-define-key 'visual ggtags-mode-map (kbd "C-]")
              #'ggtags-find-tag-dwim)
            (evil-define-key 'normal ggtags-mode-map (kbd "C-]")
              #'ggtags-find-tag-dwim))
          ;; enable ggtags in all c common mode buffers
          (add-hook 'c-mode-common-hook #'apm-ggtags-setup)))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gnuplot
  :ensure t)

(use-package google-this
  :ensure t
  :commands (google-this google-error))

(use-package goto-addr
  :defer t
  :init (add-hook 'prog-mode-hook #'goto-address-prog-mode))

(use-package gruvbox-dark-medium-theme
  :ensure gruvbox-theme)

(use-package gud
  :defer t
  :init (add-hook 'gud-mode-hook #'gud-tooltip-mode))

(use-package helm-flx
  :ensure t
  :config (helm-flx-mode 1))

(use-package helm
  :ensure t
  :diminish helm-mode
  :after helm-flx ; enable hlm-flx before helm
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
            (helm-mode 1)
            (helm-adaptive-mode 1)
            (define-key isearch-mode-map (kbd "M-o") 'helm-occur-from-isearch)
            ;; integrate with evil
            (with-eval-after-load 'evil
              (define-key evil-ex-map "b " 'helm-mini)
              (define-key evil-ex-map "bd " 'kill-buffer)
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

(use-package helm-company
  :ensure t
  :defer t
  :bind (:map company-active-map ("C-/" . helm-company)))

(use-package helm-dash
  :ensure t
  :after helm
  :defer t
  :defines (helm-dash-docsets)
  :init (unless (executable-find "sqlite3")
          (pk-install-package "sqlite3")))

(use-package helm-flyspell
  :ensure t
  ;; use instead of ispell-word which evil binds to z=
  :bind (([remap ispell-word] . helm-flyspell-correct)))

(use-package helm-fuzzier
  :ensure t
  :after helm
  :config (helm-fuzzier-mode 1))

(use-package helm-projectile
  :ensure t
  :after projectile
  :config (progn
            (setq projectile-switch-project-action 'helm-projectile)
            (helm-projectile-on)))

(use-package helm-swoop
  :ensure t
  :commands (helm-swoop helm-multi-swoop))

(use-package helm-unicode
  :ensure t)

(use-package helm-xref
  :ensure t
  :config (setq xref-show-xrefs-function #'helm-xref-show-xrefs))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-function)
         ("C-h k" . helpful-key)))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode 1))

;; autogenerate a .clang_complete if there is an associated .clang_complete.in
(defun apm-autogenerate-clang-complete ()
  "Autogenerate a .clang_complete if needed when opening a project."
  (when (and (fboundp 'projectile-project-root)
             ;; handle if not in project by returning nil
             (not (null (condition-case nil
                            (projectile-project-root)
                          (error nil))))
             (cl-every #'identity (mapcar #'(lambda (f)
                                              (file-exists-p (concat
                                                              (file-name-as-directory
                                                               (projectile-project-root))
                                                              f)))
                                          '(".clang_complete.in" "Makefile"))))
    (projectile-with-default-dir (projectile-project-root)
      (shell-command "make .clang_complete"))))

(defun apm-irony-cdb-clang-complete--auto-generate-clang-complete (_command &rest _args)
  "Try and autogenerate a .clang_complete (_COMMAND _ARGS are ignored)."
  (apm-autogenerate-clang-complete))

(use-package irony
  :ensure t
  :defer t
  :diminish irony-mode
  :commands (irony-mode irony--find-server-executable irony-install-server)
  :init (progn
          (unless (executable-find "cmake")
            (pk-install-package "cmake"))
          (unless (executable-find "clang-4.0")
            (pk-install-package "clang-4.0"))
          (unless (file-exists-p "/usr/lib/llvm-4.0/include/clang-c/Index.h")
            (pk-install-package "libclang-4.0-dev"))
          ;; try and install if not already installed
          (unless (condition-case nil
                      (irony--find-server-executable)
                    (error nil))
            (call-interactively #'irony-install-server))
          (advice-add 'irony-cdb-clang-complete :before 'apm-irony-cdb-clang-complete--auto-generate-clang-complete)
          (add-hook 'c-mode-hook 'irony-mode)
          (add-hook 'c++-mode-hook 'irony-mode)
          (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package irony-eldoc
  :ensure t
  :defer t
  :init (add-hook 'irony-mode-hook #'irony-eldoc))

(use-package ispell
  :defer t
  :init (progn
          ;; windows specific config
          (when (eq system-type 'windows-nt)
            (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
          ;; use aspell if can be found
          (when (executable-find "aspell")
            ;; use gb dictionary via aspell if available
            (setq ispell-program-name "aspell"
                  ispell-dictionary "british"
                  ispell-extra-args '("--sug-mode=ultra")))
          (add-hook 'text-mode-hook #'ispell-minor-mode)))

(use-package jenkins
  :ensure t
  :commands (jenkins)
  ;; don't set jenkins-api-token here - do it in custom.el so it is not checked
  ;; into git
  :config (setq jenkins-hostname "http://cw-jenkins/jenkins/"
                jenkins-username "amurray"
                jenkins-viewname "RelX"))

(use-package js2-mode
  :ensure t
  :defer t
  :init (setq-default js2-basic-offset 2))

(defun apm-emacs-lisp-mode-setup ()
  "Setup Emacs Lisp mode."
  ;; make imenu list each package for easy navigation - from
  ;; https://github.com/jwiegley/use-package/issues/80#issuecomment-46687774
  (when (string= buffer-file-name (expand-file-name "init.el" "~/dot_emacs.d"))
    (add-to-list
     'imenu-generic-expression
     '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)))
  (with-eval-after-load 'helm-dash
    (setq-local helm-dash-docsets '("Emacs Lisp"))))

(use-package lisp-mode
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'apm-emacs-lisp-mode-setup))

(defun apm-log-edit-insert-yasnippet-template ()
  "Insert the default template with Summary and Author."
  (interactive)
  (when (or (called-interactively-p 'interactive)
            (log-edit-empty-buffer-p))
    (yas-expand-snippet "${1:Summary of this change}

${2:Longer description of this change}

${3:Ticket: #${4:XXXX}}")))

(use-package log-edit
  :defer t
  :init (progn
          (with-eval-after-load 'evil
            (evil-set-initial-state 'log-edit-mode 'insert))
          (add-hook 'log-edit-hook 'apm-log-edit-insert-yasnippet-template)
          (remove-hook 'log-edit-hook 'log-edit-insert-message-template)))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package magit-svn
  :ensure t)

(use-package magithub
  :ensure t
  :after magit
  :init (progn
          (eval-when-compile (require 'magithub))
          (setq-default magithub-api-timeout 3))
  :config (magithub-feature-autoinject t))

(use-package make-mode
  ;; don't show major mode name
  :init (add-hook 'makefile-mode-hook #'(lambda () (setq mode-name nil))))

(use-package mallard-mode
  :ensure t
  :defer t)

(use-package mallard-snippets
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (unless (executable-find markdown-command)
            (pk-install-package "markdown")))

(defun apm-meghanada-mode-setup ()
  "Setup meghanada-mode."
  (unless (file-exists-p (meghanada--locate-server-jar))
    (meghanada-install-server)))

(use-package meghanada
  :ensure t
  :init (progn
          (add-hook 'meghanada-mode-hook #'apm-meghanada-mode-setup)
          (add-hook 'java-mode-hook 'meghanada-mode))
  :config (setq meghanada-use-company t
                meghanada-use-flycheck t
                meghanada-auto-start t))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :diminish modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package no-littering
  :ensure t
  :config (progn
            (eval-when-compile
              (require 'recentf))
            (with-eval-after-load 'recentf
              (add-to-list 'recentf-exclude no-littering-var-directory)
              (add-to-list 'recentf-exclude no-littering-etc-directory))))

(use-package nxml-mode
  ;; enable 'folding' with nxml-mode
  :init (progn
          (require 'hideshow)
          (require 'sgml-mode)

          (add-to-list 'hs-special-modes-alist
                       '(nxml-mode
                         "<!--\\|<[^/>]*[^/]>"
                         "-->\\|</[^/>]*[^/]>"

                         "<!--"
                         sgml-skip-tag-forward
                         nil))

          (add-hook 'nxml-mode-hook 'hs-minor-mode))
  :config (setq nxml-slash-auto-complete-flag t))

(use-package org
  :ensure org-plus-contrib
  :pin org
  :config (progn
            (setq org-agenda-files (mapcar #'expand-file-name
                                           '("~/Dropbox/Orgzly/personal.org"
                                             "~/Dropbox/Orgzly/cohda.org"))
                  ;; don't indent org document sections etc
                  org-adapt-indentation nil
                  org-imenu-depth 4
                  org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "BLOCKED(b@)" "DEFERRED(D@)" "|" "DONE(d!)")
                                      (sequence "|" "CANCELLED(c@)" "DELEGATED(G@)"))
                  org-goto-interface 'outline-path-completion)
            ;; set up org-babel integration for plantuml
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((plantuml .t)))))

(defun apm-org-agenda-file-notify (_event)
  "Rebuild appointments when _EVENT specifies any org agenda files change."
  (org-agenda-to-appt t))

(use-package org-agenda
  :config (progn
            ;; when modifying agenda files make sure to update appt
            (require 'filenotify)
            (dolist (file org-agenda-files)
              (file-notify-add-watch file '(change) #'apm-org-agenda-file-notify))
            ;; rebuild appointments now
            (org-agenda-to-appt t)))

(use-package org-capture
  :after org
  :config (setq org-capture-templates '(("t" "Todo" entry (file "~/Dropbox/Orgzly/cohda.org")
                                         "* TODO %?")
                                        ("p" "Project" entry (file "~/Dropbox/Orgzly/cohda.org")
                                         "* %?"))))

(defun apm-org-clock-heading ()
  "Create `org-clock-heading' by truncating if needed."
  (s-truncate 8 (nth 4 (org-heading-components))))

(defvar apm-org-clock-notification nil)

(defun apm-org-clock-clear-notification ()
  "Clear any existing org clock notification."
  (when apm-org-clock-notification
    (notifications-close-notification apm-org-clock-notification)
    (setq apm-org-clock-notification nil)))

(defun apm-org-clock-warn-notification-action (_id action)
  "For notification ID handle ACTION."
  (pcase action
    ("ignore" (setq apm-org-clock-notification 'ignore))
    ("default" (progn
                 (raise-frame)
                 (make-frame-visible)
                 (select-frame-set-input-focus (selected-frame))
                 (org-mru-clock-in)
                 (apm-org-clock-clear-notification)))))

(defun apm-org-clock-warn-if-not-clocked-in ()
  "Warn if not currently clocked in."
  (eval-when-compile
    (require 'org-clock))
  (when (and (null org-clock-current-task)
             (not (eq apm-org-clock-notification 'ignore)))
    ;; show a notification but keep it persistent - don't show more than one
    (if (require 'notifications nil t)
        (progn
          (when apm-org-clock-notification
            (notifications-close-notification apm-org-clock-notification))
          (setq apm-org-clock-notification
                (notifications-notify :title "You're not clocked in!"
                                      :body "Click to select a task or choose ignore..."
                                      :actions '("ignore" "Ignore"
                                                 "default" "Select one")
                                      :on-action #'apm-org-clock-warn-notification-action
                                      :on-close #'(lambda (id reason)
                                                    (when (= id apm-org-clock-notification)
                                                      (setq apm-org-clock-notification nil)))))))))

(use-package org-clock
  :after org
  ;; assume idle after 5 minutes
  :config (progn
            (setq org-clock-idle-time nil
                  ;; truncate clock heading in modeline
                  org-clock-heading-function #'apm-org-clock-heading
                  ;; save running clock and all history when exiting emacs
                  org-clock-persist t
                  ;; resume clocking task on clock-in if the clock is open
                  org-clock-in-resume t
                  ;; persist clock data into Dropbox
                  org-clock-persist-file (expand-file-name "~/Dropbox/Orgzly/org-clock-save.el")
                  ;; insert a CLOSED timestamp when TODOs are marked DONE
                  org-log-done 'time)
            (unless (executable-find "xprintidle")
              (pk-install-package "xprintidle"))
            (setq org-clock-x11idle-program-name "xprintidle")
            ;; reload any saved org clock information on startup
            (org-clock-persistence-insinuate)
            ;; notify if not clocked in
            (run-with-timer 60 60 #'apm-org-clock-warn-if-not-clocked-in)
            ;; ensure when clocking in we close any existing notification
            (add-hook 'org-clock-in-hook #'apm-org-clock-clear-notification)))

(use-package org-clock-convenience
  :ensure t
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-mru-clock
  :ensure t)

(use-package org-notify
  :ensure org-plus-contrib
  :after org
  :config (progn
            (org-notify-start)
            (org-notify-add 'default '(:time "24h" :actions -notify/window :duration 600))
            (org-notify-add 'default '(:time "60m" :actions -notify/window :period "2m" :duration 600))
            (org-notify-add 'default '(:time "15m" :actions -notify/window :period "2m" :duration 120))))

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :init (add-hook 'org-mode-hook 'org-table-sticky-header-mode))

(use-package ob-plantuml
  :after plantuml-mode
  :config (with-eval-after-load 'plantuml-mode
            (setq org-plantuml-jar-path plantuml-jar-path)))

(defun apm-paradox-set-github-token (_no-fetch)
  "Load `paradox-github-token' from authinfo."
  (require 'epa-file)
  (require 'auth-source)
  (eval-when-compile
    (require 'paradox-github))
  (if (file-exists-p "~/.authinfo.gpg")
      (let ((authinfo-result (car (auth-source-search
                                   :max 1
                                   :host "github.com"
                                   :port "paradox"
                                   :user "paradox"
                                   :require '(:secret)))))
        (let ((paradox-token (plist-get authinfo-result :secret)))
          (setq paradox-github-token (if (functionp paradox-token)
                                         (funcall paradox-token)
                                       paradox-token))))
    (alert "No github token found in ~/.authinfo.gpg")))

(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :init (setq paradox-execute-asynchronously nil)
  :config (progn
            (paradox-enable)
            (advice-add 'paradox-list-packages :before 'apm-paradox-set-github-token)))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

(use-package pcap-mode
  :ensure t
  :mode ("\\.pcapng\\'" . pcap-mode))

(use-package pdf-tools
  :ensure t
  ;; only try and install when needed
  :mode ("\\.pdf\\'" . pdf-tools-install))

(use-package plantuml-mode
  :ensure t
  :mode ("\\.p\\(lant\\)?uml\\'" . plantuml-mode)
  :config (progn
            (setq plantuml-jar-path (expand-file-name "~/plantuml.jar"))
            (unless (file-exists-p plantuml-jar-path)
              (alert (format "plantuml not found at %s" plantuml-jar-path)))))

;; Disable audible and visible bell in favor of flashing the mode line instead
(defun apm-powerline-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'powerline-active1)
  (run-with-timer 0.1 nil 'invert-face 'powerline-active1)
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))


(use-package powerline
  :ensure t
  :config (progn
            (setq powerline-default-separator 'wave)
            (setq visible-bell nil)
            (setq ring-bell-function #'apm-powerline-visible-bell)))

(use-package prog-mode
  :config (progn
            (when (boundp 'prettify-symbols-unprettify-at-point)
              ;; show original text when point is over a prettified symbol
              (setq prettify-symbols-unprettify-at-point 'right-edge))
            ;; prettify symbols (turn lambda -> λ)
            (global-prettify-symbols-mode 1)))

(use-package projectile
  :ensure t
  :defer t
  :defines (projectile-enable-caching)
  :diminish projectile-mode
  :bind (("C-x C-m" . projectile-compile-project)
         ("C-x C-g" . projectile-find-file))
  :init (progn
          (setq projectile-enable-caching t)
          (projectile-mode 1))
  :config (progn
            (add-to-list 'projectile-project-root-files "configure.ac")
            (add-to-list 'projectile-project-root-files ".clang_complete")
            (add-to-list 'projectile-project-root-files ".clang_complete.in")
            (add-to-list 'projectile-project-root-files "AndroidManifest.xml")
            (with-eval-after-load 'helm
              (setq projectile-completion-system 'helm))))

(use-package projectile-speedbar
  :ensure t
  :after sr-speedbar
  :config (progn
            (defadvice helm-projectile-find-file (after locate-file activate)
              (if (sr-speedbar-exist-p)
                  (projectile-speedbar-open-current-buffer-in-tree)))
            (defadvice speedbar-item-load (after speedbar-highlight-file activate)
              (projectile-speedbar-open-current-buffer-in-tree))))

(use-package psvn
  :ensure t
  :config (setq svn-status-state-mark-modeline nil))

(use-package python
  :defer t
  :init (setq-default python-indent-offset 4))

(defun apm-racer-mode-setup ()
  "Setup racer-mode."
  (eval-when-compile
    (require 'racer))
  (unless (file-exists-p racer-cmd)
    (alert "cargo install racer?"))
  (unless (file-exists-p racer-rust-src-path)
    (alert (format "git clone https://github.com/rust-lang/rust.git %s"
                   (file-name-directory (directory-file-name racer-rust-src-path)))))
  (racer-mode 1))

(use-package racer
  :ensure t
  :defer t
  :init (progn
          (setq racer-rust-src-path (expand-file-name "~/rust/src"))
          (add-hook 'rust-mode-hook #'apm-racer-mode-setup)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init (dolist (hook '(css-mode-hook html-mode-hook))
          (add-hook hook #'rainbow-mode)))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

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

(use-package simple
  :defer t
  ;; save whatever is in the system clipboard to the kill ring before killing
  ;; something else into the kill ring
  :init (progn
          (setq save-interprogram-paste-before-kill t)
          (add-hook 'text-mode-hook #'visual-line-mode)))

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
  :init (progn
          (smartparens-global-mode 1)
          ;; use smartparens in strict mode for programming and ielm
          (add-hook 'prog-mode-hook #'smartparens-strict-mode)
          (add-hook 'ielm-mode-hook #'smartparens-strict-mode))
  :config (progn
            (require 'smartparens-config)
            (require 'smartparens-latex)
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
              (sp-local-pair mode "{" nil :post-handlers '((apm-c-mode-common-open-block "RET"))))
            ;; don't try and complete tag end - breaks nxml completion etc
            (sp-local-pair 'nxml-mode "<" ">" :actions '(:rem insert))))

(use-package solarized-theme
  :ensure t
  :disabled t
  :config (progn
            (setq x-underline-at-descent-line t)
            (setq solarized-distinct-fringe-background t)
            (setq solarized-scale-org-headlines nil)
            (setq solarized-use-variable-pitch nil)
            (load-theme 'solarized-light t)))

(use-package spaceline-config
  :ensure spaceline
  :after powerline
  :config (progn
            (setq spaceline-workspace-numbers-unicode t
                  spaceline-window-numbers-unicode t
                  spaceline-responsive t)
            (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
            (spaceline-helm-mode)
            (spaceline-info-mode)))

(use-package spaceline-all-the-icons
  :ensure t
  :config (progn
            (setq spaceline-all-the-icons-icon-set-bookmark 'heart
                  spaceline-all-the-icons-icon-set-modified 'circle
                  spaceline-all-the-icons-icon-set-dedicated 'pin
                  spaceline-all-the-icons-flycheck-alternate 'slim
                  spaceline-all-the-icons-highlight-file-name t
                  spaceline-all-the-icons-hide-long-buffer-path t
                  spaceline-all-the-icons-separator-type 'none)
            (spaceline-toggle-all-the-icons-bookmark-on)
            (spaceline-toggle-all-the-icons-dedicated-on)
            (spaceline-all-the-icons--setup-anzu)
            (spaceline-all-the-icons--setup-git-ahead)
            (spaceline-all-the-icons--setup-paradox)
            (spaceline-all-the-icons--setup-package-updates)
            (spaceline-all-the-icons-theme)))

(use-package sr-speedbar
  :ensure t
  :bind (("<f8>" . sr-speedbar-toggle))
  :config (setq sr-speedbar-right-side nil))

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package systemd
  :ensure t)

(use-package tracwiki-mode
  :ensure t
  :defer t
  :commands tracwiki
  :config (tracwiki-define-project
           "mk2"
           "http://projects.cohda.wireless:8000/trac/mk2"))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package web-mode
  :ensure t
  :defer t
  :commands web-mode
  :init (progn
          ;; use smartparens instead
          (setq web-mode-enable-auto-pairing nil))
  :mode ("\\.php\\'" . web-mode))

(use-package which-func
  :config (which-function-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace
  :diminish whitespace-mode)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode 1))

(use-package xref
  :config (with-eval-after-load 'evil
            (define-key evil-visual-state-map (kbd "C-]") #'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "C-]") #'xref-find-definitions)
            (define-key evil-normal-state-map (kbd "M-*") #'xref-pop-marker-stack)))

(use-package zenburn-theme
  :ensure t
  :disabled t
  :config (progn
            (load-theme 'zenburn t)
            ;; nicer looking modeline
            (set-face-attribute 'mode-line nil :box '(:line-width 2 :color "gray30")
                                :weight 'normal :foreground "#C8F7C8" :background "gray20")
            (set-face-attribute 'mode-line-inactive nil :box '(:line-width 2 :color "gray30")
                                :weight 'normal :foreground "gray70")
            (set-face-attribute 'powerline-active2 nil :background "gray32")
            (set-face-attribute 'powerline-active1 nil :background "gray32" :weight 'normal)
            (set-face-attribute 'mode-line-buffer-id nil :foreground "#FFECBA" :weight 'bold)
            ;; dim inactive modeline
            (set-face-attribute 'powerline-inactive2 nil :background "gray20")
            (set-face-attribute 'powerline-inactive1 nil :background "gray32")))

(provide 'init)

;;; init.el ends here

;;; init.el --- Starting point for Alex Murray's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;;; Package management
(require 'package)

;; use https for both melpa and gelpa
(eval-and-compile
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/"))))

;; this is done automatically in 27 etc
(if (or (version< emacs-version "27")
        (not package--initialized))
    (package-initialize))

(defvar use-package-enable-imenu-support t)
;; must be set before loading use-package
(setq use-package-enable-imenu-support t)

;; Bootstrap `use-package' from melpa
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package))

;; uncomment to debug package loading times
;; (setq use-package-verbose t)
;; compute stats to see what is taking so long on init
;; (setq use-package-compute-statistics t)

;; load no-littering as soon as possible during init so it can hook as many
;; paths as possible
(use-package no-littering
  :ensure t
  :config
  (eval-when-compile
    (require 'recentf))
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(use-package system-packages
  :ensure t
  :config
  (setq system-packages-package-manager 'apt)
  (setq system-packages-noconfirm t)
  (setq system-packages-use-sudo t))

;; customisations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load custom but ignore error if doesn't exist
(load custom-file t)

(use-package alert
  :ensure t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

(use-package doom-modeline
  :preface
  (eval-and-compile
    (require 'erc-track))
  (defun apm-erc-stylize-buffer-name (name)
    (let ((channel (assoc (get-buffer name) erc-modified-channels-alist)))
      (if channel
          (erc-make-mode-line-buffer-name name (car channel) (cddr channel)
                                          (cadr channel))
        name)))
  :ensure t
  :custom
  (doom-modeline-mu4e t)
  (doom-modeline-irc t)
  (doom-modeline-irc-buffers t)
  (doom-modeline-irc-stylize #'apm-erc-stylize-buffer-name)
  (doom-modeline-buffer-encoding nil)
  :init
  (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; better keyword lightlighting
  (add-to-list 'doom-themes-base-faces '(erc-keyword-face :foreground yellow :weight 'bold))
  (doom-themes-org-config)
  (load-theme 'doom-one t))

;; used in some of my yasnippet snippets
(defun apm-camelize (s &optional delim)
  "Convert under_score string S to CamelCase string with optional DELIM."
  (interactive "s")
  (mapconcat 'identity (mapcar
                        #'(lambda (word) (capitalize (downcase word)))
                        (split-string s (if delim delim "_"))) ""))

(defvar apm-preferred-emacs-version "27.1")
(when (version< emacs-version apm-preferred-emacs-version)
  (alert (format "Emacs version too old - please run %s or newer"
                 apm-preferred-emacs-version)
         :severity 'high))


;;; General settings etc from C source so no package to associate settings
;;; with...

;; use pipes for subprocess communication
(setq process-connection-type nil)
;; performance increases as per https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; personalisation
(setq user-full-name "Alex Murray")

;; enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; tabs are never ok
(setq-default indent-tabs-mode nil)

;; set a reasonable fill and comment column
(setq-default fill-column 75)
(setq-default comment-column 75)

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't try and use dialog boxes
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

;; disable menu, tool and scroll-bars, show time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
;; Show line column numbers in mode line
(line-number-mode 1)
(column-number-mode 1)
;; Show buffer size in mode line
(size-indication-mode 1)
;; don't use gtk style tooltips since are intrusive
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))
(blink-cursor-mode -1)
;; adopt the default from emacs-28
(setq frame-title-format
      '(multiple-frames "%b" ("%b - GNU Emacs")))
(setq icon-title-format frame-title-format)

;; make emacs less laggy
(setq inhibit-compacting-font-caches t)

(set-language-environment "UTF-8")

;; prompt when trying to switch out of a dedicated window
(setq switch-to-buffer-in-dedicated-window 'prompt)

;; ensure scrolling forwards / backwards preserves original location such that
;; they undo each other
(setq scroll-preserve-screen-position 'always)

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
  :config
  (setq save-abbrevs t)
  (setq-default abbrev-mode t))

(use-package ace-window
  :ensure t
  :after avy
  :defer t
  :bind (("C-x o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :hook ((text-mode . adaptive-wrap-prefix-mode)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :config (global-aggressive-indent-mode))

(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (all-the-icons-install-fonts)))

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package android-mode
  :ensure t
  :defer t
  :diminish android-mode
  :commands android-mode
  :hook java-mode
  :init
  (setq android-mode-sdk-dir (expand-file-name "~/android-sdk-linux/"))
  ;; change prefix so doesn't conflict with comment-region
  (setq android-mode-key-prefix (kbd "C-c C-m")))

(use-package ansi-color
  ;; show colours correctly in shell
  :config (ansi-color-for-comint-mode-on))

(use-package anzu
  :ensure t
  :diminish anzu-mode global-anzu-mode
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace-regexp)
         ("C-M-%" . anzu-query-replace)))

(use-package apm-misc
  :load-path "lisp/"
  :bind (("C-c b l" . apm-browse-lp-bug-at-point)))

(use-package apparmor-mode
  :ensure t)

(use-package appt
  :preface
  (defun apm-appt-notify (min-to-appt _new-time msg)
    "Notify for appointment at MIN-TO-APPT for _NEW-TIME with MSG."
    ;; the arguments may be lists or not so always use them as lists
    (unless (listp min-to-appt)
      (setq min-to-appt (list min-to-appt)))
    (unless (listp msg)
      (setq msg (list msg)))
    (alert (string-join msg "\n")
           :title (format "Appointment(s) in %s minutes" (string-join min-to-appt ", "))
           :icon "/usr/share/icons/HighContrast/32x32/status/appointment-soon.png"))
  :config
  (setq appt-disp-window-function #'apm-appt-notify)
  (appt-activate 1))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :diminish auto-revert-mode
  :config (global-auto-revert-mode 1))

(use-package auctex
  :ensure t
  :preface
  (defun apm-latex-mode-setup ()
    "Tweaks and customisations for LaTeX mode."
    ;; Enable source-correlate for Control-click forward/reverse search.
    (TeX-source-correlate-mode 1)
    ;; enable math mode in latex
    (LaTeX-math-mode 1)
    ;; Enable reftex
    (turn-on-reftex))
  :defer t
  :commands (LaTeX-math-mode TeX-source-correlate-mode)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook ((LaTeX-mode . apm-latex-mode-setup))
  :init
  (setq-default TeX-auto-save t)
  (setq-default TeX-parse-self t)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (setq-default reftex-plug-into-AUCTeX t)
  (setq-default TeX-source-correlate-start-server t))

(use-package auth-source
  ;; prefer gnome-keyring via Login keyring, encrypted auth source to non-encrypted
  :init
  (require 'secrets)
  (setq auth-sources '("~/.authinfo" "secrets:Login" "~/.netrc")))

(use-package avy
  :ensure t
  :bind (("C-\\" . avy-goto-char-timer))
  :config
  ;; dim text when avy is active
  (setq avy-background t)
  ;; insert chars infront of target rather than over target
  (setq avy-style 'pre))

(use-package beginend
  :ensure t
  :diminish beginend-global-mode
  :config
  ;; beginend defines lots of different modes so diminish them all
  (dolist (m beginend-modes)
    (diminish (cdr m)))
  (beginend-global-mode 1))

(use-package bbdb
  :ensure t
  :config (bbdb-initialize 'message 'mu4e))

(use-package blacken
  :ensure t
  :init (unless (executable-find "black")
          (alert "Please install the black snap")))

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package bug-reference
  :defer t
  :hook ((prog-mode . bug-reference-prog-mode)
         (erc-mode . bug-reference-mode)
         (mu4e-view-mode . bug-reference-mode)
         (org-mode . bug-reference-mode))
  :preface (defun apm-bug-reference-url-format ()
             (let ((prefix (match-string-no-properties 1))
                   (id (match-string-no-properties 3)))
               (cond ((or (string-prefix-p "lp" prefix t)
                          (string-prefix-p "bug" prefix t))
                      (format "https://bugs.launchpad.net/bugs/%s" id))
                     ((string-prefix-p "CVE" prefix)
                      (format "https://ubuntu.com/security/CVE-%s.html" id))
                     ((string-prefix-p "USN" prefix)
                      (format "https://ubuntu.com/security/notices/USN-%s" id))
                     (t (error (concat "Unknown bug prefix '%s'" prefix))))))
  :init
  (eval-when-compile
    (require 'bug-reference))
  (setq bug-reference-url-format #'apm-bug-reference-url-format
        bug-reference-bug-regexp "\\<\\(\\([Ll][Pp]:?\\|bug\\) #?\\|CVE[ -]\\|USN[ -]\\)\\([0-9][0-9-]*\\)\\>"))

(use-package calendar
  :custom (calendar-week-start-day 1))

(use-package cargo
  :ensure t
  :defer t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package cc-mode
  :defer t
  :preface
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
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook ((c-mode-common . apm-c-mode-common-setup))
  :config
  ;; from https://www.kernel.org/doc/html/v5.0/process/coding-style.html
  (defvar c-syntactic-element)
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1) c-basic-offset)))
  ;; Add upstream kernel style which uses actual tabs
  (c-add-style "linux-tabs-only"
               '("linux" (c-offsets-alist
                          (arglist-cont-nonempty
                           c-lineup-gcc-asm-reg
                           c-lineup-arglist-tabs-only)))))

(use-package apm-c
  :load-path "lisp/"
  :preface
  (defun apm-c-mode-setup ()
    "Tweaks and customisations for `c-mode'."
    (let ((filename (buffer-file-name)))
      (if (and filename (string-match-p (expand-file-name "~/git/apparmor-kernel") filename))
          ;; upstream linux kernel style actually uses tabs... urgh
          (progn
            (setq indent-tabs-mode t)
            (setq c-basic-offset 8)
            (c-set-style "linux-tabs-only"))
        ;; default to normal linux style
        (c-set-style "linux")))
    ;; always show trailing whitespace
    (setq show-trailing-whitespace t)
    ;; ensure fill-paragraph takes doxygen @ markers as start of new
    ;; paragraphs properly
    (setq paragraph-start "^[ ]*\\(//+\\|\\**\\)[ ]*\\([ ]*$\\|@param\\)\\|^\f"))
  :hook (((c-mode c++-mode) . apm-c-mode-setup))
  :config
  ;; treat linux styles as safe for local variable
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux))
  (add-to-list 'safe-local-variable-values '(c-indentation-style . linux-tabs-only)))

(use-package check-cves-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :mode ("check-cves\\..*\\'" . check-cves-mode))

(use-package company
  :ensure t
  :diminish company-mode
  ;; Use Company for completion
  :bind (("C-<tab>" . company-complete-common)
         :map company-mode-map
         ([remap completion-at-point] . company-complete-common)
         ([remap complete-symbol] . company-complete-common)
         ([remap indent-for-tab-command] . company-indent-or-complete-common))
  :init (global-company-mode 1)
  :config
  (setq tab-always-indent 'complete)
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

  ;; put most often used completions at stop of list
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-anaconda
  :ensure t
  :defer t
  :after company
  :init (add-to-list 'company-backends #'company-anaconda))

(use-package company-auctex
  :ensure t
  :defer t
  :hook ((LaTeX-mode . company-auctex-init)))

(use-package company-dabbrev
  :after company
  ;; keep original case
  :config (setq company-dabbrev-downcase nil))

(use-package company-math
  :ensure t
  :defer t
  :after company
  ;; Add backend for math characters
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-posframe
  :ensure t
  :diminish company-posframe-mode
  :hook (after-init . company-posframe-mode))

(use-package company-reftex
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))

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
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error))

(use-package consult
  :ensure t
  :bind (
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-s i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s o" . consult-outline)
         ("M-y". consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)         )
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq-default consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :ensure t
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package cov
  :load-path "vendor/"
  :defer t
  :diminish cov-mode
  :hook ((prog-mode . cov-mode))
  :init (use-package elquery
          :ensure t))

(use-package crontab-mode
  :ensure t)

(use-package cua-base
  ;; use CUA mode for rectangle selections etc but not copy/paste etc
  :init (cua-selection-mode 1))

(use-package cve-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts"
  :mode (("CVE-[[:digit:]]\\{4\\}-[[:digit:]]\\{4,\\}\\'" . cve-mode)
         ("00boilerplate.*\\'" . cve-mode))
  :hook ((cve-mode . flycheck-mode)))

;; show suspicious c constructs automatically
(use-package cwarn
  :diminish cwarn-mode
  :init (global-cwarn-mode 1))

(use-package debian-el
  :ensure t)

(use-package cvelist
  :load debian-cvelist
  :load-path "~/ubuntu/git/security-tracker/conf/"
  :mode ("list" . debian-cvelist-mode))

(use-package debbugs
  :ensure t)

(use-package define-word
  :ensure t
  :bind (("C-c d" . define-word-at-point)
         ("C-c D" . define-word)))

(use-package diff-mode
  :mode (("\\.debdiff\\'" . diff-mode)))

(use-package disk-usage
  :ensure t)

(use-package display-fill-column-indicator
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)))

(use-package dpkg-dev-el
  :ensure t
  :after flyspell
  :hook (debian-changelog-mode . flyspell-mode))

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(use-package diff-hl
  :ensure t
  ;; Integrate with Magit and highlight changed files in the fringe of dired
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config (global-diff-hl-mode 1))

(use-package disaster
  :ensure t
  :bind ((:map c-mode-base-map ("C-c d" . disaster))))

(use-package dts-mode
  :ensure t)

(use-package edebug-inline-result
  :ensure t
  :defer t
  :custom (edebug-inline-result-backend 'posframe)
  :hook (edebug-mode . edebug-inline-result-mode))

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
  :init
  ;; electric indent, layout and pair modes to make more IDE like
  (electric-indent-mode 1)
  (electric-layout-mode 1)
  (electric-pair-mode 1))

(use-package elisp-def
  :ensure t
  :defer t
  :diminish elisp-def-mode
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(use-package emojify
  :ensure t
  :diminish emojify-mode
  :demand t
  :bind (("C-c e" . emojify-insert-emoji))
  :config
  ;; display emojis using images since looks nicer
  (setq emojify-display-style 'image)
  ;; replace github and unicode emojis (github ones are used in mattermost)
  (setq emojify-emoji-styles '(github unicode))
  ;; echo the actual underlying character to the minibuffer when point is
  ;; over them so we don't mess with the displayed buffer itself
  (setq emojify-point-entered-behaviour 'echo)
  (global-emojify-mode 1))

(use-package erc
  :ensure t
  :preface
  (eval-when-compile
    (require 'erc-log)
    (require 'erc-match))

  (defun apm-prompt-to-connect-to-irc ()
    "Prompt to connect to irc."
    (interactive)
    (when (y-or-n-p "Connect to IRC? ")
      ;; connect to matterircd on localhost and oftc and freenode via znc
      (erc :server "localhost" :port "6667" :nick "alexmurray")
      (erc :server "irc.oftc.net" :port "6667" :nick "amurray")
      (erc-tls :server "znc.secret.server" :port "7076"
               :nick "amurray" :password (concat "amurray/libera:"
                                                 (auth-source-pick-first-password
                                                  :user "amurray"
                                                  :port "7076")))))
  (defgroup apm-erc nil
    "apm's erc customisations."
    :group 'erc)

  ;; face to show in header line when disconnected
  (defface apm-erc-header-line-disconnected
    '((t (:foreground "black" :background "indianred")))
    "Face to use when ERC has been disconnected."
    :group 'apm-erc)

  (defun apm-erc-update-header-line-show-disconnected ()
    "Use a different face in the header-line when disconnected."
    (erc-with-server-buffer
      (unless (erc-server-process-alive)
        'apm-erc-header-line-disconnected)))

  (defun apm-erc-find-logfile ()
    "Find and open the current `erc-mode` buffers logfile."
    (interactive)
    (when (and (eq major-mode 'erc-mode) erc-log-mode)
      (find-file-other-window (erc-current-logfile))))

  (defun apm-erc-nicks ()
    "Returns the list of possible nicks from `erc-nick'."
    (if (listp erc-nick)
        erc-nick
      (list erc-nick)))

  (defun apm-occur-in-erc (&optional regexp)
    "Find matches of REGEXP in all erc buffers.
With a prefix argument, will default to looking for all
`erc-keywords' and mentions of `erc-nick'."
    (interactive
     (list
      (let ((regex  (concat "\\(" (regexp-opt erc-keywords) "\\|"
                            (concat "\\(^\\|[^<]\\)" (regexp-opt (apm-erc-nicks)) "\\([^>]\\|$\\)")
                            "\\)")))
        (read-string "Regexp: "
                     (substring-no-properties
                      (or (cond ((region-active-p)
                                 (buffer-substring (region-beginning) (region-end)))
                                (current-prefix-arg
                                 regex)
                                (t
                                 (word-at-point)))
                          ""))))))
    (let ((erc-buffers nil))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (eq major-mode 'erc-mode)
                     (not (erc-server-buffer-p)))
            (push buffer erc-buffers))))
      (multi-occur erc-buffers regexp)))

  (defun apm-erc-browse-url-from-channel-topic ()
    "Find urls in erc-channel-topic and offer to visit via `browse-url'."
    (interactive)
    (let ((topic erc-channel-topic)
          (urls nil))
      (with-temp-buffer
        (insert topic)
        (goto-char (point-min))
        (while (re-search-forward "https?://" nil t)
          (push (thing-at-point 'url t) urls)))
      (if urls
          (browse-url (completing-read "URL: " urls))
        (user-error "No URLs listed in channel topic"))))

  (defun apm-erc-lookup-nick (nick)
    ;; if this is a matterircd buffer then query via launchpadid since they
    ;; are used as nicks there
    (if (eq 'matterircd (erc-network))
        (apm-eudc-lookup-launchpadid nick)
      (apm-eudc-lookup-nick nick)))

  :hook ((after-init . apm-prompt-to-connect-to-irc))
  :bind (:map erc-mode-map
              ("C-c f e" . apm-erc-find-logfile)
              ("M-s e" . apm-occur-in-erc))
  :config
  (eval-and-compile
    (require 'erc-button)
    (require 'erc-desktop-notifications)
    (require 'erc-fill)
    (require 'erc-join)
    (require 'erc-log)
    (require 'erc-match)
    (require 'erc-networks)
    (require 'erc-notify)
    (require 'erc-services)
    (require 'erc-track))

  (setq erc-user-full-name user-full-name)
  (setq erc-nick (list user-login-name "alexmurray"))
  (setq erc-prompt-for-nickserv-password nil)
  ;; add basic libera.chat support
  (add-to-list 'erc-networks-alist
               '(Libera.Chat "libera.chat"))
  (add-to-list 'erc-nickserv-alist
               '(Libera.Chat
                 "NickServ!NickServ@services.libera.chat"
                 ;; Libera.Chat also accepts a password at login, see the `erc'
                 ;; :password argument.
                 "This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose"
                 "NickServ"
                 "IDENTIFY" nil nil
                 ;; See also the 901 response code message.
                 "You\\s-are\\s-now\\s-identified\\s-for\\s-"))
  ;; nickserv password for liber and oftc
  (dolist (network '((Libera.Chat . "irc.libera.chat")
                     (OFTC . "irc.oftc.net")))
    (let ((login (auth-source-user-and-password (cdr network))))
      (if (null login)
          ;; secret-tool store --label='(car network) IRC NickServ' host
          ;; (cdr network) user USER then enter password
          (alert (format "Please store %s NickServ password in secret store" (cdr network)))
        (add-to-list 'erc-nickserv-passwords `(,(car network) ((,(car login) . ,(cadr login))))))))

  (setq erc-autojoin-timing 'ident)

  ;; since we connect to oftc directly, we need to autojoin channels there
  ;; - not needed for libera (since we use ZNC) or canonical matterircd
  (setq erc-autojoin-channels-alist '(("oftc.net" "#apparmor" "#debian-security")))
  (setq erc-fill-function #'erc-fill-static)
  ;; account for really long names
  (setq erc-fill-static-center 22)
  ;; this fits on a dual horizontal split on my laptop
  (setq erc-fill-column 115)

  ;; use sensible buffer names with server as well
  (setq erc-rename-buffers t)

  ;; try harder to reconnect but wait longer each time since it may take a
  ;; while to get a DHCP lease etc
  (setq erc-server-auto-reconnect t)
  (setq erc-server-reconnect-attempts 5)
  (setq erc-server-reconnect-timeout 30)

  (add-to-list 'erc-modules 'button)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'services)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)

  ;; format nicknames to show if user has voice(+), owner (~), admin (&),
  ;; operator (@)
  (setq erc-format-nick-function 'erc-format-@nick)
  (setq erc-pals '("joemcmanus" "jdstrand" "mdeslaur" "sbeattie" "jjohansen" "jj" "sarnold" "ChrisCoulson" "leosilva" "msalvatore" "ebarretto" "pfsmorigo" "emitorino" "deafpool" "Avital" "dan" "sid"))
  (setq erc-notify-list erc-pals)

  (setq erc-keywords '("cve" "vulnerability" "apparmor" "seccomp" "exploit" "security" "esm" "@here" "@all" "@channel" "@security"))

  ;; when joining don't bring to front
  (setq erc-join-buffer 'bury)

  (setq erc-track-switch-direction 'importance)
  (setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"
                                  ;; channel mode (324), creation
                                  ;; time (329), topic (332), topic
                                  ;; who time (333), names (353), no
                                  ;; chan modes (477)
                                  "324" "329" "332" "333" "353" "477"))

  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-showcount t)
  ;; emacs channels are noisy
  (setq erc-track-exclude '("#emacs" "#emacsconf"))
  (setq erc-track-shorten-function nil)
  ;; ensure our nick highlighted with erc-hl-nicks gets picked up by
  ;; erc-track
  (with-eval-after-load 'erc-hl-nicks
    (dolist (nick (apm-erc-nicks))
      (add-to-list 'erc-track-faces-priority-list
                   `(,(erc-hl-nicks-make-face nick) erc-current-nick-face))))

  (add-to-list 'erc-nick-popup-alist
               '("Directory" . (apm-erc-lookup-nick nick)))
  ;; only hide join / part / quit for those who are idle for more
  ;; than 10 hours (ie are using a bouncer)
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (setq erc-lurker-threshold-time (* 10 60 60))

  ;; hide channel mode (324), creation time (329), topic (332), topic
  ;; who time (333), names (353) - see
  ;; https://www.alien.net.au/irc/irc2numerics.html
  (setq erc-hide-list '("324" "329" "332" "333" "353"))

  (setq erc-log-channels-directory "~/.emacs.d/erc/logs")
  (setq erc-log-insert-log-on-open nil)
  (setq erc-log-file-coding-system 'utf-8)
  (setq erc-log-write-after-send t)
  (setq erc-log-write-after-insert t)
  (setq erc-save-buffer-on-part t)

  ;; log mentions when away
  (add-to-list 'erc-log-matches-types-alist
               '(current-nick . "ERC Mentions"))

  (unless (file-exists-p erc-log-channels-directory)
    (mkdir erc-log-channels-directory t))

  (erc-autojoin-mode 1)

  (erc-spelling-mode 1)

  ;; change header line face when disconnected
  (setq erc-header-line-face-method
        #'apm-erc-update-header-line-show-disconnected)

  ;; make sure any privmsg (which is via query buffers) show up as urgent
  ;; in track list
  (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
        (setq ad-return-value (intern "erc-current-nick-face"))
      ad-do-it)))

(use-package erc-goodies
  :ensure erc
  ;; ensure this is set and we don't inadvertently unset it
  :bind (:map erc-mode-map ("C-c C-c" . nil))
  :config (setq erc-interpret-controls-p t))

(use-package erc-hl-nicks
  :ensure t
  :after erc
  ;; erc-match will highlight pals with erc-pal-face so override that
  ;; for pretty colors
  :config (setq erc-hl-nicks-skip-faces
                (delete "erc-pal-face" erc-hl-nicks-skip-faces)))

(use-package erc-image
  :ensure t
  :after erc
  :config
  (add-to-list 'erc-modules 'image)
  (erc-update-modules))

(use-package erc-matterircd
  :load-path "vendor/"
  :after erc
  :config
  ;; don't clutter view with context ids and make sure emojify doesn't try
  ;; and emojify over the top of us otherwise the keymap added by
  ;; erc-matterircd's use of erc-button get lost
  (setq erc-matterircd-replace-context-id
        (propertize "↩" 'emojify-inhibit t))
  (setq erc-matterircd-updatelastviewed-on-buffer-switch t)
  (setq erc-matterircd-server "chat.canonical.com")
  (setq erc-matterircd-team "canonical")
  (let ((token (auth-source-pick-first-password :host "matterircd")))
    (if (null token)
        (alert (format "Please store matterircd token in secret store with :host matterircd"))
      (setq erc-matterircd-password (concat "token=" token))))
  (add-to-list 'erc-modules 'matterircd)
  (erc-update-modules))

(use-package erc-view-log
  :ensure t
  :config
  ;; colorise nicks better
  (with-eval-after-load 'erc-hl-nicks
    (setq erc-view-log-nickname-face-function #'erc-hl-nicks-make-face))
  (add-to-list 'auto-mode-alist
                       `(,(format "%s/.*\\.[log|txt]"
                                  (regexp-quote
                                   (expand-file-name
                                    erc-log-channels-directory))) . erc-view-log-mode)))

(use-package ldap
  :ensure-system-package (ldapsearch . ldap-utils)
  :config
  ;; Store password using secret-tool as follows:
  ;; secret-tool store --label='Canonical LDAP' host ldaps://ldap.canonical.com
  ;; then enter PASSWORD
  (setq ldap-host-parameters-alist
        `(("ldaps://ldap.canonical.com"
           base "ou=staff,dc=canonical,dc=com"
           binddn "cn=Alex Murray,ou=staff,dc=canonical,dc=com"
           auth-source t)))
  (setq ldap-default-host "ldaps://ldap.canonical.com"))

(use-package link-hint
  :ensure t
  :bind
  ("C-c C-l C-o" . link-hint-open-link)
  ("C-c C-l C-c" . link-hint-copy-link))

(use-package eudc
  :after ldap
  :config
  (eval-when-compile
    (require 'ldap))
  (eudc-set-server ldap-default-host 'ldap)
  ;; better display of custom canonical ldap attributes
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillanickname . "IRC Nick"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillacustom1 . "Team"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillacustom3 . "Timezone Name"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(mozillacustom4 . "UTC Offset"))
  (add-to-list 'eudc-user-attribute-names-alist
               '(launchpadid . "Launchpad ID"))
  ;; keep byte-compiler happy
  (defvar eudc-bob-generic-keymap nil)
  ;; add support for querying up the hierarchy via manager
  (defvar apm-eudc-bob-query-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [return] 'apm-eudc-query-at-point)
      map))
  (set-keymap-parent apm-eudc-bob-query-keymap eudc-bob-generic-keymap)
  (add-to-list 'eudc-attribute-display-method-alist '("jpegphoto" . eudc-display-jpeg-inline))
  (add-to-list 'eudc-attribute-display-method-alist '("manager" . apm-eudc-display-query))
  (add-to-list 'eudc-attribute-display-method-alist '("utc offset" . apm-eudc-display-utc-offset))
  (add-to-list 'eudc-attribute-display-method-alist '("timezone name" . apm-eudc-display-timezone))
  (add-to-list 'eudc-attribute-display-method-alist '("launchpad id" . apm-eudc-display-launchpadid))
  (add-to-list 'eudc-attribute-display-method-alist '("irc nick" . apm-eudc-display-nick))
  (defun apm-eudc-query-at-point ()
    (interactive)
    (let ((id (eudc-bob-get-overlay-prop 'query)))
      (unless id (error "No query here"))
      (let ((name))
        (save-match-data
          (if (not (and (string-match "cn=\\([^,]*\\)" id)
                        (setq name (match-string 1 id))))
              (error "Failed to extract cn from id %s" id)
            (eudc-display-records (eudc-query `((cn . ,name)))))))))

  (defun apm-eudc-display-timezone (timezone)
    "Display TIMEZONE with current local time."
      (insert (concat timezone
                      " ["
                      (current-time-string nil timezone)
                      "]")))

  (defun apm-eudc-display-utc-offset (offset)
    "Display OFFSET with current local time."
    (let ((hours (string-to-number (substring offset 0 3)))
          (mins (string-to-number (substring offset 3))))
      (insert (concat offset
                      " ["
                      (current-time-string
                       (time-add (current-time) (* (+ (* hours 60) mins) 60)) t)
                      "]"))))

  (defun apm-eudc-display-launchpadid (id)
    "Display ID as a clickable URL."
    (eudc-display-url (concat "https://launchpad.net/~" id)))

  (defun apm-eudc-display-nick (nick)
    "Display NICK as using colors from erc-hl-nicks."
    (insert (propertize nick
                        'face
                        (erc-hl-nicks-make-face nick))))

  (defun apm-eudc-display-query (query)
    "Display QUERY as an interactive element."
    (eudc-bob-make-button query apm-eudc-bob-query-keymap nil (list 'query query)))

  (defun apm-eudc-lookup-email (&optional email)
    (interactive (list
                  (let ((initial
                         ;; remove any opening / closing angle brackets if
                         ;; present
                         (replace-regexp-in-string "[<>]" ""
                                                   (substring-no-properties (or (thing-at-point 'email) "")))))
                    (read-string "Email address: " initial))))
    (eudc-display-records (eudc-query  `((email . ,email)))))

  (defun apm-eudc-lookup-nick (&optional nick)
    (interactive
     (list
      ;; some buffers are read-only in which case `word-at-point' returns a
      ;; read-only string so need to remove properties
      (let ((initial (substring-no-properties (or (word-at-point) ""))))
        (if (eq major-mode 'erc-mode)
            (completing-read "Nick: " (erc-get-channel-nickname-list)
                             nil nil initial)
          (read-string "Nick: " initial)))))
    (eudc-display-records (eudc-query  `((mozillaNickName . ,nick)))))

  (defun apm-eudc-lookup-launchpadid (&optional id)
    (interactive
     (list
      ;; some buffers are read-only in which case `word-at-point' returns a
      ;; read-only string so need to remove properties
      (let ((initial (substring-no-properties (or (word-at-point) ""))))
        (if (eq major-mode 'erc-mode)
            (completing-read "Id: " (erc-get-channel-nickname-list)
                             nil nil initial)
          (read-string "Id: " initial)))))
    (eudc-display-records (eudc-query  `((launchpadid . ,id))))))

(use-package eshell
  :defer t
  :preface
  (defun apm-eshell-mode-setup ()
    "Initialise 'eshell-mode'."
    (eval-when-compile
      (require 'em-cmpl))
    (eshell-cmpl-initialize))
  :commands eshell
  :hook ((eshell-mode . apm-eshell-mode-setup)))

(use-package eshell-syntax-highlighting
  :ensure t
  :config (eshell-syntax-highlighting-global-mode 1))

(use-package eterm-256color
  :ensure t
  :hook ((term-mode . eterm-256color-mode))
  :init (unless (file-exists-p "~/.terminfo/e/eterm-color")
          (make-directory "~/.terminfo/e/" t)
          (shell-command "tic /snap/emacs/current/usr/share/emacs/27.0.50/etc/e/eterm-color.ti")))

(use-package executable
  :hook ((after-save . executable-make-buffer-file-executable-if-script-p)))

(use-package exec-path-from-shell
  :ensure t
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize)))

(use-package eyebrowse
  :ensure t
  :custom (eyebrowse-keymap-prefix (kbd "C-c w"))
  :config
  (eyebrowse-mode 1)
  ;; binds to M-1 M-2 etc to easily switch
  (eyebrowse-setup-opinionated-keys))

(use-package files
  :bind ("C-c r" . revert-buffer))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands flycheck-add-next-checker
  :ensure-system-package (cppcheck shellcheck)
  :init (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :hook ((prog-mode . flycheck-mode))
  :config
  ;; use lsp checker via cquery instead
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))
  (setq-default flycheck-display-errors-delay 0.2))

(use-package flycheck-checkbashisms
  :ensure t
  :after flycheck
  :ensure-system-package (checkbashisms . devscripts)
  :config (flycheck-checkbashisms-setup))

(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :ensure-system-package clang
  :config
  (setq flycheck-clang-analyzer-executable "clang")
  (flycheck-clang-analyzer-setup)
  ;; automatically sets itself up as next checker after lsp so undo
  ;; that so is instead after cppcheck
  (ignore-errors
    (delete '(warning . clang-analyzer)
            (flycheck-checker-get 'lsp 'next-checkers)))
  (flycheck-add-next-checker 'c/c++-cppcheck '(t . clang-analyzer)))

(use-package flycheck-coverity
  :ensure t
  :after flycheck-clang-analyzer
  :init (unless (executable-find "cov-run-desktop")
          (alert "cov-run-desktop not found - is it installed?"))
  :config
  (flycheck-coverity-setup)
  (flycheck-add-next-checker 'clang-analyzer '(t . coverity)))

(use-package flycheck-package
  :ensure t
  :after flycheck
  :config (flycheck-package-setup))

(use-package flycheck-pod
  :load-path "vendor/")

(use-package flycheck-posframe
  ;; :ensure t
  :load-path "~/git/flycheck-posframe"
  :after (flycheck posframe)
  :hook ((flycheck-mode . flycheck-posframe-mode))
  :config (flycheck-posframe-configure-pretty-defaults))

(use-package flycheck-relint
  :ensure t
  :after (flycheck flycheck-package)
  :config (flycheck-relint-setup))

(use-package flycheck-rust
  :ensure t
  :config (flycheck-rust-setup))

(use-package flyspell
  :defer t
  :diminish flyspell-mode
  ;; improve performance by making flyspell less chatty
  :config (setq flyspell-issue-message-flag nil)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package forge
  :ensure t
  :after magit)

(use-package fuzzy
  :ensure t)

(use-package gcmh
  :ensure t
  :disabled t
  :diminish gcmh-mode
  :config (gcmh-mode 1))

(use-package gdb-mi
  :defer t
  :init
  ;; use gdb-many-windows by default
  (setq-default gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  (setq-default gdb-show-main t))

(use-package gif-screencast
  :ensure t
  :ensure-system-package (scrot gifsicle))

(use-package gitconfig-mode
  :ensure t
  :defer t)

(use-package gitignore-mode
  :ensure t
  :defer t)

(use-package gnu-elpa
  :defer t
  :ensure t)

(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package gnuplot
  :ensure t)

(use-package gnus-art
  :ensure gnus
  :config
  ;; add custom highlighting to gnus for launchpad security bugs
  (add-to-list 'gnus-emphasis-alist
               (list "\\(Private security bug reported\\)" 1 1 'error))
  (add-to-list 'gnus-emphasis-alist
               (list "\\(This bug is a security vulnerability\\)" 1 1 'warning))
  ;; github PRs I have been requested to review
  (add-to-list 'gnus-emphasis-alist
               (list "\\(requested your review on:\\)" 1 1 'error))
  ;; and for build failure emacs
  (add-to-list 'gnus-emphasis-alist
               (list "\\(State: failed to build\\)" 1 1 'error))
  ;; don't fill long lines as breaks tables in emails
  (setq gnus-treat-fill-long-lines nil)
  ;; disable image loading
  (setq gnus-inhibit-images t)
  ;; discourse emails use --- as signature separator
  (setq gnus-signature-separator '("^---? $" "^---? *$"))
  ;; prefer plain text if have both html it
  (setq mm-discouraged-alternatives '("text/html" "text/richtext")))

(use-package gnus-dired
  :ensure gnus
  :config
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (define-advice gnus-dired-mail-buffers (:around (orig-fun &rest args) include-message-mode-derived-buffers)
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (append (apply orig-fun args) (nreverse buffers))))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(use-package gnus-patch
  :load-path "vendor/")

(use-package goggles
  :ensure t
  :config (goggles-mode 1))

(use-package good-scroll
  :ensure t
  :config (good-scroll-mode 1))

(use-package go-mode
  :ensure t)

(use-package goto-addr
  :defer t
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

(use-package goto-line-preview
  :ensure t
  :config (global-set-key [remap goto-line] 'goto-line-preview))

(use-package gud
  :defer t
  :hook ((gud-mode . gud-tooltip-mode)))

(use-package help-at-pt
  :custom (help-at-pt-display-when-idle t))

(use-package helpful
  :ensure t
  :bind (("C-h a" . helpful-symbol)
         ("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package hideshow
  :diminish hs-minor-mode
  ;; use hs-minor-mode in programming and mail composing - TODO - get it
  ;; working during mail viewing as well to be able to hide quoted bits
  ;; - something like:
  ;; :config (add-to-list 'hs-special-modes-alist '(mu4e-view-mode "^>" "^[^^]"))
  :hook ((prog-mode message-mode) . hs-minor-mode))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-todo
  :ensure t
  :defer t
  :functions global-hl-todo-mode
  :config
  (add-to-list 'hl-todo-keyword-faces '("@todo" . "#cc9393"))
  (global-hl-todo-mode))

(use-package hungry-delete
  :ensure t
  :diminish hungry-delete-mode
  :config (global-hungry-delete-mode 1))

(use-package imenu
  :bind (("M-i" . imenu)))

(use-package iscroll
  :ensure t
  :diminish iscroll-mode
  :hook ((erc-mode .  iscroll-mode)))

(use-package ispell
  :defer t
  :ensure-system-package aspell
  :custom
  (ispell-program-name "aspell")
  (ispell-dictionary "australian")
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-silently-savep t))

(use-package keypression
  :ensure t)

(use-package journalctl-mode
  :ensure t)

(use-package jq-mode
  :ensure t)

(use-package js
  :custom (js-indent-level 2))

(use-package js2-mode
  :ensure t
  :defer t
  :init (setq-default js2-basic-offset 2))

(use-package json-mode
  :ensure t)

(use-package lxd-tramp
  :ensure t)

(use-package lp
  :load-path "/snap/gitlptools/current"
  :after magit
  :init (unless (executable-find "git-lp-open")
          (alert "Please install the gitlptools snap")))

(use-package lsp-mode
  :ensure t
  ;; wants yasnippet by default and complains if not already available
  :after yasnippet
  :ensure-system-package (pyls . python3-pyls)
  :hook ((prog-mode . lsp)
         (yaml-mode . lsp))
  :custom
  (lsp-headerline-breadcrumb-enable t)
  (lsp-eldoc-enable-hover nil)
  :config
  (lsp-register-custom-settings
   ;; for gopls disable -mod=readonly for now
   '(("gopls.allowModfileModifications" t t))))

(use-package lsp-imenu
  :ensure lsp-mode
  :hook ((lsp-after-open . lsp-enable-imenu)))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :hook ((java-mode . lsp-java-enable)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map (([remap xref-find-definitions] . lsp-find-definition)
                               ([remap xref-find-references] . lsp-find-references))))

(use-package magit
  :ensure t
  :preface
  :defer t
  :bind (("C-x g g" . magit-status)
         ("C-x g d" . magit-dispatch)
         ("C-x g f" . magit-file-dispatch) ))

(use-package magit-patch-changelog
  :ensure t)

(use-package magit-popup
  ;; whilst magit doesn't need this anymore, other packages do and magit
  ;; doesn't provide it anymore so explicitly add it
  :ensure t)

(use-package mallard-mode
  :ensure t
  :defer t)

(use-package marginalia
  :ensure t
  :hook ((after-init . marginalia-mode)))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :ensure-system-package markdown)

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode 1))

(use-package meson-mode
  :ensure t)

(use-package message
  ;; use standard completion UI for message completion
  :config (setq message-expand-name-standard-ui t))

(use-package message-attachment-reminder
  :ensure t)

(use-package mml-sec
  :config
  (define-advice mml-secure-find-usable-keys
      (:after-until (context name usage &optional justone) prompt-for-missing-key)
    (when (y-or-n-p (format "No %s key for %s; do you want to manually choose one? "
                            usage name))
      (let ((keys (epa-select-keys context
                                   (format "Choose a key for %s " name))))
        (if (and justone (> (length keys) 0))
            (mml-secure-select-keys context name keys usage)
          keys))))
  (setq mml-secure-key-preferences '((OpenPGP
                                      (sign)
                                      (encrypt
                                       ("security@ubuntu.com"
                                        "88E9530BCBDDC200517B5EB0F498D2D9DE7DAD9C" ;amurray
                                        "4C20C06B5D8BDE688854D28A51DBDC58CC559573" ;jdstrand
                                        "A6063BB5602309A43C8EBBD42F099E8D005E81F4" ;sbeattie
                                        "50C4A0DDCF31E452CEB19B516569D855A744BE93" ;mdeslaur
                                        "D92678445CD9BFA56C74EC8EC4057122C10A23E6" ;jjohansen
                                        "415010F1BA23C8C720DFB1F5F32172599D8D2E97" ;sarnold
                                        "44DFFFE4C1A008E83229E205611FBDECD5946E0F" ;ChrisCoulson
                                        "7FE79B445728C8EA0042839E45BCE75B840B1F69" ;leosilva
                                        "C196DEF7A9097968763884D1772835433D285D7D" ;msalvatore
                                        "776FCD6573E7712AC111E3B0FBD3F310B1C9C4C1" ;joe
                                        "08AA09489F9DC266D9046E05A897FD7E8600E018" ;markmorlino
                                        "0ADCB2CFA6B3532E80641CD2906788EB31A737FF" ;ebarretto
                                        "410774CCB1C9851EB8BFECADA179EB5276C81B4C" ;jmbl
                                        "3E2CC11CC9ED7154183B9615C2A82596AE602046" ;vineetha1
                                        "9027444394FA0EEB52BDE0B26D4A7990BDE2CC66" ;pfsmorigo
                                        "C8726381716BC3D27B868575CB8F16FBD3A64F82" ;emitorino
                                        "9673BF778F45A143CAF5FC32B46763D063A1DBEC" ;Avital
                                        "7AB7FF87BB30FEBADD1BADDCD6073960E299350D" ;sid
                                        "5F2395C9FCE8A66078A8E9CDCCAACB01128F5657" ;dan
                                        "5D3A333BBF6CEB840A3446BCA7D23FB0E778DEDF"))) ;kyrofa
                                     (CMS
                                      (sign)
                                      (encrypt)))))

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package moinmoin-mode
  :load-path "vendor/")

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package mqtt-mode
  :ensure t
  :ensure-system-package (mosquitto_pub .  mosquitto-clients))

(use-package mu4e
  :load-path "/snap/maildir-utils/current/share/emacs/site-lisp/mu4e/"
  :bind (("C-c m" . mu4e))
  :config
  (eval-and-compile
    (require 'mu4e-compose)
    (require 'mu4e-message)
    (require 'mu4e-utils))
  ;; some hacky bits so we can do an async forward as attachment
  (defvar apm-mu4e-compose-mode-hook-orig)
  (defvar apm-mu4e-compose-forward-as-attachment-orig)

  (defun apm-mu4e-compose-forward-as-attachment-2 ()
    (setq mu4e-compose-forward-as-attachment
          apm-mu4e-compose-forward-as-attachment-orig)
    (setq mu4e-compose-mode-hook
          apm-mu4e-compose-mode-hook-orig))

  (defun apm-mu4e-compose-forward-as-attachment ()
    "Forward the message as an attachment."
    (interactive)
    (setq apm-mu4e-compose-mode-hook-orig mu4e-compose-mode-hook)
    (setq apm-mu4e-compose-forward-as-attachment-orig mu4e-compose-forward-as-attachment)
    (setq mu4e-compose-forward-as-attachment t)
    (setq mu4e-compose-mode-hook '(apm-mu4e-compose-forward-as-attachment-2))
    (mu4e-compose-forward))

  (defun apm-mu4e-contact-process (contact)
    (unless (string-match-p
             "\\(no-?reply\\|bugs.launchpad.net\\|keule.canonical.com\\|lillypilly.canonical.com\\|docs.google.com\\|gapps.canonical.com\\|forum.snapcraft.io\\)"
             contact)
      contact))

  (defvar apm-mu4e-spammers '("duke.abbaddon@gmail.com"
                              "redmine@mantykora.net"
                              "jira@nine.ch"
                              "providing@babiel.com"
                              "jshaymac@gmail.com"
                              "jira@clockworkers.atlassian.net"
                              "advisories@auraredeye.zendesk.com"
                              "gohr@mail.continue.de"
                              "m1-en0on.jp"))

  ;; TODO: consider using imapfilter
  (defun apm-mu4e-refile-message (msg)
    (let ((mailing-list (mu4e-message-field msg :mailing-list))
          (subject (mu4e-message-field msg :subject)))
      (cond
       ((and (not (null mailing-list))
             (or (string-match-p "unsubscribe notification$" subject)
                 (string-match-p "^.* post from .* requires approval" subject)
                 (string-match-p "uncaught bounce notification" subject)
                 (string-match-p "Bounce action notification" subject)
                 (string-match-p "moderator request(s) waiting" subject)))
        mu4e-trash-folder)
       ((cl-some #'(lambda (e) (mu4e-message-contact-field-matches msg :from e)) apm-mu4e-spammers)
        "/Spam")
       ((or (string-match-p "^\\[\\(For Your Eyes Only\\|Limited Time Offer\\)\\].*" subject)
            (string-match-p "no deposit" subject))
        "/Spam")
       ((or (mu4e-message-contact-field-matches msg :from "do-not-reply@trello.com")
            (mu4e-message-contact-field-matches msg :from "bounce@websense.com")
            (mu4e-message-contact-field-matches msg :from "comments-noreply@docs.google.com")
            (mu4e-message-contact-field-matches msg :from "info@ubuntuforums.org")
            (mu4e-message-contact-field-matches msg :from "root@snakefruit.canonical.com")
            (mu4e-message-contact-field-matches msg :from "esm-notifications@canonical.com")
            (mu4e-message-contact-field-matches msg :from "notify@google.com")
            (mu4e-message-contact-field-matches msg :from "noreply\\+chat.canonical.com@canonical.com")
            (mu4e-message-contact-field-matches msg :from "jira@warthogs.atlassian.net"))
        mu4e-trash-folder)
       ((or (string-match-p "^\\[.* Wiki\\] \\(Update of\\|New attachment added to page\\)" subject)
            (string-match-p "We just published a new Production deploy for ubuntusecuritypodcast" subject)
            (string-match-p "Archive is broken! (kernel ABI check failure)" subject)
            (string-match-p "Undelivered Mail Returned to Sender" subject)
            (string-match-p "^.* post from .* requires approval" subject)
            (string-match-p "VINCE  \\[VU#[0-9]+\\] New Post in Case Discussion" subject)
            (string-match-p "^\\[Build #[0-9]+]" subject)
            (string-match-p "^CVE webbot process errors$" subject))
        mu4e-trash-folder)
       ((mu4e-message-contact-field-matches msg :from "root@lillypilly.canonical.com")
        "/lillypilly")
       ((mu4e-message-contact-field-matches msg :from "root@keule.canonical.com")
        "/keule")
       ((not (null mailing-list))
        (concat "/Lists/" (mu4e-get-mailing-list-shortname mailing-list)))
       ;; store emails about outdated dependencies or reviews should get
       ;; trashed
       ((and (mu4e-message-contact-field-matches msg :from "Snap Store")
             (string-match-p "\\(built from outdated Ubuntu kernel\\|was built with outdated Ubuntu packages\\|contains outdated Ubuntu packages\\|^Manual review requested for version\\|^Manual review for .* version .* requested$\\)" subject))
        mu4e-trash-folder)
       ((or (mu4e-message-contact-field-matches msg :from "Snap Store")
            (string-match-p "^\\(R[eE]: \\)?Manual review \\(for .* version .* requested\\|requested for version\\)" subject)
            (string-match-p "^Package declaration update: " subject)
            (string-match-p "Store upload \\(scan \\)?failed for.*$" subject)
            (string-match-p "Store authorization failed for.*$" subject))
        "/snap-store")
       ((or (string-match-p "^\\[Bug [0-9]+\\] \\(R[eE]: \\)?.*linux.*proposed tracker$" subject)
            (string-match-p "^\\[Bug [0-9]+\\] \\(\\(R[eE]:\\|\\[NEW\\]\\) \\)?request of \\(\\(create\\|merge\\)_\\(production_suite\\|project\\)\\|\\(copy\\|publish\\|delete\\)_package\\|gensnapshot\\)" subject))
        mu4e-trash-folder)
       ((or (string-match-p "^\\(R[eE]: \\)?\\[Bug " subject)
            (mu4e-message-contact-field-matches msg :from "bugs.launchpad.net"))
        "/launchpad-bugs")
       ((or (string-match-p "^\\(R[eE]: \\)?\\[Bug " subject)
            (mu4e-message-contact-field-matches msg :from "answers.launchpad.net"))
        "/launchpad-answers")
       ((mu4e-message-contact-field-matches msg :from "bugs.debian.org")
        "/debian-bugs")
       ((string-match-p "^\\(R[eE]: \\)?\\[Merge\\]" subject)
        "/merge-requests")
       ((or (mu4e-message-contact-field-matches msg :to "distros@vs.openwall.org")
            (mu4e-message-contact-field-matches msg :cc "distros@vs.openwall.org")
            (mu4e-message-contact-field-matches msg :to "linux-distros@vs.openwall.org")
            (mu4e-message-contact-field-matches msg :cc "linux-distros@vs.openwall.org"))
        "/Lists/linux-distros")
       ((mu4e-message-contact-field-matches msg :to "opensuse-security-announce@opensuse.org")
        "/Lists/opensuse-security-announce")
       ((mu4e-message-contact-field-matches msg :to "newsbox@idg.com")
        "/Lists/newsbox-idg")
       ((string-match-p "^Cron .* ~/bin/scripts-diff.sh$" subject)
        mu4e-trash-folder)
       ((mu4e-message-contact-field-matches msg :from "atpi.com")
        "/Travel")
       ((or (mu4e-message-contact-field-matches msg :from "rt@admin.canonical.com")
            (mu4e-message-contact-field-matches msg :to "rt@admin.canonical.com"))
        "/canonical-is")
       ((and (mu4e-message-contact-field-matches msg :from "security@ubuntu.com")
             (or (string-match-p "^Missing kernel CVE merge commits$" subject)
                 (string-match-p "^Kernel USN needed$" subject)))
        mu4e-trash-folder)
       ((or (mu4e-message-contact-field-matches msg :from "lillypilly.canonical.com")
            (and (mu4e-message-contact-field-matches msg :from "noreply@canonical.com")
                 (string-match-p "\\(contains outdated Ubuntu packages\\|built from outdated Ubuntu kernel\\)" subject))
            (and (mu4e-message-contact-field-matches msg :from "noreply@launchpad.net")
                 (string-match-p "^\\[.*\\] .* build of .* in ubuntu .*$" subject)))
        mu4e-trash-folder)
       (t "/Archive"))))

  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-sent-folder   "/Sent"
        mu4e-drafts-folder "/Drafts"
        mu4e-trash-folder  "/Trash")
  (setq mu4e-maildir-shortcuts
        '(("/Archive"              . ?a)
          ("/Drafts"               . ?d)
          ("/INBOX"                . ?i)
          ("/Sent"                 . ?s)
          ("/Spam"                 . ?j)
          ("/Trash"                . ?t)))

  (setq mu4e-user-mailing-lists '(;; ubuntu
                                  ("launchpad-announce.lists.ubuntu.com" . "launchpad-announce")
                                  ("ubuntu-devel.lists.ubuntu.com" . "ubuntu-devel")
                                  ("ubuntu-devel-announce.lists.ubuntu.com" . "ubuntu-devel-announce")
                                  ("ubuntu-hardened.lists.ubuntu.com" . "ubuntu-hardened")
                                  ("ubuntu-release.lists.ubuntu.com" . "ubuntu-release")
                                  ("ubuntu-security-announce.lists.ubuntu.com" . "ubuntu-security-announce")

                                  ;; canonical
                                  ("canonical-allhands.lists.canonical.com" . "canonical-allhands")
                                  ("canonical-announce.lists.canonical.com" . "canonical-announce")
                                  ("canonistack-announce.lists.canonical.com" . "canonistack-announce")
                                  ("canonical-snapcraft.lists.canonical.com" . "canonical-snapcraft")
                                  ("roadmap-sprint.lists.canonical.com" . "roadmap-sprint")
                                  ("ue-leads.lists.canonical.com" . "ue-leads")

                                  ;; security
                                  ("security-announce.lists.apple.com" . "apple-security-announce")
                                  ("bugtraq.list-id.securityfocus.com" . "bugtraq")
                                  ("debian-security-announce.lists.debian.org" . "debian-security-announce")
                                  ("fulldisclosure.lists.seclists.org" . "fulldisclosure")
                                  ("kernel-hardening.lists.openwall.com" . "kernel-hardening")
                                  ("officesecurity.lists.freedesktop.org" . "Officesecurity")
                                  ("opensuse-security-announce.opensuse.org" . "opensuse-security-announce")
                                  ("oss-security.lists.openwall.com" . "oss-security")
                                  ("rhsa-announce.redhat.com" . "rhsa-announce")
                                  ("security-ceph.com" . "security-ceph")
                                  ("xen-security-issues.lists.xenproject.org" . "xen-security-issues")
                                  ("xorg-security.lists.x.org" . "xorg-security")))

  (setq mu4e-refile-folder #'apm-mu4e-refile-message)
  (setq mu4e-get-mail-command "mbsync -a")
  ;; needed for mbsync
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-headers-sort-direction 'ascending)

  ;; show all since often get duplicates via multiple mailing lists so want
  ;; to be able to see them all in general
  (setq mu4e-headers-skip-duplicates nil)

  (setq mu4e-update-interval 300)

  ;; cite with better formatting
  (setq message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:\n")
  (setq message-citation-line-function #'message-insert-formatted-citation-line)

  ;; kill message buffer after sending rather than burying
  (setq message-kill-buffer-on-exit t)

  (setq mu4e-compose-reply-to-address "alex.murray@canonical.com"
        user-mail-address "alex.murray@canonical.com"
        user-full-name  "Alex Murray")
  ;; encrypt to self
  (setq epg-user-id "alex.murray@canonical.com")
  (setq mml-secure-openpgp-encrypt-to-self t)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mu4e-compose-signature nil)

  (setq mu4e-contact-process-function #'apm-mu4e-contact-process)

  ;; quick bookmarks to easily delete unwanted emails
  (add-to-list 'mu4e-bookmarks
               '(:name "Ubuntu kernel process spam"
                       :query "subject:bug and subject:linux and subject:proposed and subject:tracker"
                       :key ?k))
  (add-to-list 'mu4e-bookmarks
               '(:name "Snap Store spam"
                       :query "(from:\"Snap Store\" or from:noreply@canonical.com) and not to:alex.murray@canonical.com and subject:\"outdated Ubuntu packages\" or subject:\"outdated Ubuntu kernel\""
                       :key ?s))
    (add-to-list 'mu4e-bookmarks
               '(:name "Ubuntu OEM team process bug spam"
                       :query "from:bugs.launchpad.net and subject:bug and subject:\"request of\""
                       :key ?o))
  ;; add action to view in brower
  (add-to-list 'mu4e-view-actions
               '("browser view" . mu4e-action-view-in-browser) t)
  ;; since we use the firefox snap, it has a private /tmp so use somewhere
  ;; common (home) to work-around this...
  (define-advice mu4e-action-view-in-browser (:around (orig-fun &rest args) per-user-tmp)
    (let ((temporary-file-directory (expand-file-name "~/tmp")))
      (unless (file-exists-p temporary-file-directory)
        (make-directory temporary-file-directory))
      (apply orig-fun args)))

  (setq mu4e-html2text-command 'mu4e-shr2text)
  (with-eval-after-load 'shr
    ;; html colors in shr usually look bad especially with a dark theme
    (setq shr-use-colors nil)
    ;; also fonts don't normally layout well
    (setq shr-use-fonts nil))

  (setq mu4e-split-view 'horizontal)
  (setq mu4e-headers-visible-lines 15)
  (setq mu4e-completing-read-function 'completing-read)
  (setq mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 15)
                              (:from-or-to . 22)
                              (:size . 8)
                              (:subject)))

  ;; save attachment to Downloads
  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))

  ;; attempt to show images when viewing messages
  (setq mu4e-view-show-images t)

  ;; allows to see attached patches etc more easily inline and also inline
  ;; PGP
  (setq mu4e-view-use-gnus t)

  ;; show full addresses in message view
  (setq mu4e-view-show-addresses t)

  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)

  ;; always start mu4e in the background
  (mu4e t))

(use-package mu4e-alert
  :ensure t
  :hook ((after-init . mu4e-alert-enable-mode-line-display)
         (after-init . mu4e-alert-enable-notifications))
  :config (mu4e-alert-set-default-style 'notifications))

(use-package mu4e-jump-to-list
  :ensure t)

(use-package mu4e-marker-icons
  :ensure t
  :init
  (setq mu4e-marker-icons-use-unicode t)
  (mu4e-marker-icons-mode 1))

(use-package mwheel
  :config
  ;; Scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (setq mouse-wheel-progressive-speed nil)
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000)
  (setq fast-but-imprecise-scrolling t))


(use-package nxml-mode
  ;; enable 'folding' with nxml-mode
  :init
  (require 'hideshow)
  (require 'sgml-mode)

  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (add-hook 'nxml-mode-hook 'hs-minor-mode)
  :config (setq nxml-slash-auto-complete-flag t))

(use-package minibuffer
  :config (setq completion-styles '(basic partial-completion orderless)))

(use-package orderless
  ;; for vertico
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :pin org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c C-w" . org-refile))
  ;; ensure we always load org at startup
  :demand t
  :config
  (setq org-pretty-entities t)
  ;; use org-appear instead
  (setq org-hide-emphasis-markers t)
  (setq org-directory (expand-file-name "~/org-files/"))
  (setq org-agenda-files (mapcar #'(lambda (f)
                                     (expand-file-name f org-directory))
                                 '("personal.org" "canonical.org"
                                   "inbox.org" "tickler.org" "notes.org")))
  ;; don't indent org document sections etc
  (setq org-adapt-indentation nil)
  (setq org-imenu-depth 4)
  ;; @ = add note with time
  ;; ! = record only time of state change
  ;; | = remaining keywords are final states
  (setq org-todo-keywords '((sequence "TODO(t)" "BLOCKED(b@)" "DEFERRED(D@)" "|" "DONE(d!)")
                            (sequence "|" "CANCELLED(c@)" "DELEGATED(G@)")))
  ;; ensure it is harder to inadvertently delete collapsed parts of org
  ;; documents
  (setq org-catch-invisible-edits 'smart)
  (setq org-ctrl-k-protect-subtree t)
  (add-to-list 'org-file-apps '("\\.webm\\'" . "xdg-open %s"))
  (add-to-list 'org-file-apps '("\\.aup\\'" . "audacity %s")))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package org-refile
  :ensure org-plus-contrib
  :config
  (setq org-refile-targets '(("~/org-files/canonical.org" :maxlevel . 4)
                             ("~/org-files/someday.org" :level . 1)
                             ("~/org-files/tickler.org" :maxlevel . 1)
                             ("~/org-files/notes.org" :maxlevel . 2)))
  ;; allow to refile as top-level items in files
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

;; add support for man: links in org documents
(use-package ol-man
  :ensure org-plus-contrib
  :pin org)

(use-package org-agenda
  :ensure org-plus-contrib
  :pin org
  :preface
  (defun apm-org-agenda-file-notify (_event)
    "Rebuild appointments when _EVENT specifies any org agenda files change."
    (org-agenda-to-appt t))
  (defun apm-org-agenda-skip-all-siblings-but-first-todo ()
    "Skip all but the first TODO entry."
    (let ((should-skip-entry nil))
      (unless (string= "TODO" (org-get-todo-state))
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (string= "TODO" (org-get-todo-state))
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))
  :config
  ;; when modifying agenda files make sure to update appt
  (require 'filenotify)
  (dolist (file org-agenda-files)
    (file-notify-add-watch file '(change) #'apm-org-agenda-file-notify))
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  ;; when showing agenda, jump to now
  (add-hook 'org-agenda-finalize-hook
            #'org-agenda-find-same-or-today-or-agenda 90)
  ;; rebuild appointments now
  (org-agenda-to-appt t)
  (setq org-agenda-custom-commands
        '(("i" "TODO from inbox" todo "TODO"
           ((org-agenda-files '("~/org-files/inbox.org"))))
          ("c" "TODO from canonical" todo "TODO"
           ((org-agenda-files '("~/org-files/canonical.org"))))
          ("I" "Next TODO from all" todo "TODO" (
            (org-agenda-skip-function #'apm-org-agenda-skip-all-siblings-but-first-todo))))))

(use-package org-capture
  :preface
  :after org
  :config
  (let ((inbox-org (expand-file-name "inbox.org" org-directory))
        (tickler-org (expand-file-name "tickler.org" org-directory))
        (canonical-org (expand-file-name "canonical.org" org-directory)))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,inbox-org)
             "* TODO %i%?
- %a")
            ("I" "tickler" entry (file ,tickler-org)
             "* %i%?
%U
- %a")
            ("p" "project" entry (file ,canonical-org)
             "* TODO %i%?
- %a")
            ("m" "meeting" entry (file+headline ,canonical-org "Meetings")
             "* %^{meeting title}
%^{meeting day+time}T
- %a%?")
            ("R" "snap-store-review" entry (file+olp ,canonical-org "Manual snap reviews")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("D" "snap-store-discussion" entry (file+olp ,canonical-org "snap forum store request discussions")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("T" "snap-store-tallied-processed" entry (file+olp ,canonical-org "snap forum store requests processed/tallied")
             "** %^{snap name}
- %a%?" :clock-in t :clock-keep t)
            ("P" "snapd-pr-review" entry (file+olp ,canonical-org "snapd PR reviews")
             "* [[https://github.com/snapcore/snapd/pull/%\\1][snapd PR #%^{number} %^{title}]]
- https://github.com/snapcore/snapd/pull/%\\1%?")
            ("r" "protocol" entry (file ,inbox-org)
             "* %^{Title}
Source: %u, %c
#+BEGIN_QUOTE
%i
#+END_QUOTE

%?")
            ("L" "Protocol Link" entry (file ,inbox-org)
             "* %? [[%:link][%:description]]
Captured On: %U")))))

(use-package org-clock
  :after org
  ;; ensure we always run org-clock-persistence-insinuate below
  :demand t
  :bind (("C-c g" . org-clock-goto)
         ("C-c i" . org-clock-in)
         ("C-c o" . org-clock-out))
  ;; assume idle after 5 minutes
  :ensure-system-package xprintidle
  :config
  (setq org-clock-idle-time 5
        ;; include the current clocked in task in clock reports
        org-clock-report-include-clocking-task t
        ;; save running clock and all history when exiting emacs
        org-clock-persist t
        ;; resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;; persist clock data into org-directory
        org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory)
        ;; insert a note when TODOs are marked DONE
        org-log-done 'note)
  (setq org-clock-x11idle-program-name "xprintidle")
  ;; reload any saved org clock information on startup
  (org-clock-persistence-insinuate))

(use-package org-clock-convenience
  :ensure t
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-duration
  :ensure org-plus-contrib
  ;; don't show days, only total hours as maximum value
  :config (setq org-duration-format (quote h:mm)))

(use-package orgit
  :ensure t)

(use-package org-mru-clock
  :ensure t
  :bind (("C-c s" . org-mru-clock-in))
  :demand t
  :config
  (setq org-mru-clock-format-function #'substring)
  (setq org-mru-clock-how-many 50))

(use-package org-mu4e
  :after (mu4e org)
  ;; store link to message if in header view, not to header query
  :config (setq mu4e-org-link-query-in-headers-mode nil))

(use-package org-protocol
  :ensure org-plus-contrib
  :init (let ((handler (shell-command-to-string "xdg-mime query default x-scheme-handler/org-protocol")))
          (unless (string-match "^emacsclient.*.desktop" handler)
            ;; ensure an emacsclient26.desktop file exists in
            ;; ~/.local/share/applications and that it has a MimeType
            ;; entry of x-scheme-handler/org-protocol;

            ;; then run the following to set it as the default handler:
            ;; xdg-mime default emacsclient26.desktop \
            ;; x-scheme-handler/org-protocol

            ;; also see the following
            ;; https://github.com/sprig/org-capture-extension#under-linux
            (alert "Please configure emacsclient as handler for org-protocol"))))

(use-package org-src
  :ensure org-plus-contrib
  :pin org
  :config
  ;; preserve indentation of org src blocks
  (setq org-src-preserve-indentation t))

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :diminish org-table-sticky-header-mode
  :hook ((org-mode . org-table-sticky-header-mode)))

(use-package org-timeline
  :ensure t
  :after org
  :hook ((org-agenda-finalize . org-timeline-insert-timeline)))

(use-package org-wild-notifier
  :ensure t
  :config (org-wild-notifier-mode 1))

(use-package ox-hugo
  :ensure t)

(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :init (setq paradox-execute-asynchronously nil)
  :config
  (define-advice paradox-list-packages (:before (_no-fetch) set-github-token)
    "Load `paradox-github-token' from authinfo."
    (require 'epa-file)
    (require 'auth-source)
    (eval-when-compile
      (require 'paradox-github))
    (if (file-exists-p "~/.authinfo")
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
      (alert "No github token found in ~/.authinfo")))
  (paradox-enable))

(use-package paredit
  :diminish paredit-mode
  :ensure t
  ;; don't steal occur prefix
  :bind (:map paredit-mode-map ("M-s" . nil))
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

(use-package paren
  :defer t
  :commands show-paren-mode
  :config
  (show-paren-mode 1))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

(use-package pcap-mode
  :ensure t
  :mode ("\\.pcapng\\'" . pcap-mode))

(use-package pdf-tools
  :load-path "/snap/pdf-tools/current/usr/share/emacs/site-lisp/pdf-tools"
  ;; only try and install when needed
  :mode ("\\.pdf\\'" . pdf-tools-install))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

(use-package pod-mode
  :load-path "vendor/"
  :mode ("\\.pod$" . pod-mode))

(use-package posframe
  :ensure t
  :config (setq posframe-mouse-banish nil))

(use-package prescient
  :ensure t
  :config (prescient-persist-mode 1))

(use-package prog-mode
  :config
  (when (boundp 'prettify-symbols-unprettify-at-point)
    ;; show original text when point is over a prettified symbol
    (setq prettify-symbols-unprettify-at-point 'right-edge))
  ;; prettify symbols (turn lambda -> λ)
  (global-prettify-symbols-mode 1))

(use-package projectile
  :ensure t
  :demand t
  :bind (:map projectile-mode-map ("C-x p" . projectile-command-map))
  :diminish projectile-mode
  :config
  (with-eval-after-load 'compile
    (setq compilation-buffer-name-function
          #'projectile-compilation-buffer-name)
    (setq compilation-save-buffers-predicate
          #'projectile-current-project-buffer-p))
  ;; prompt for action on project switch
  (setq projectile-switch-project-action #'projectile-commander)
  (projectile-mode 1))

(use-package python
  :defer t
  :init (setq-default python-indent-offset 4))

(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map ("C-x t" . python-pytest-dispatch))
  :config (setq python-pytest-executable "pytest-3"))

(use-package quilt
  :ensure t)

(use-package racer
  :ensure t
  :preface
  (defun apm-racer-mode-setup ()
    "Setup racer-mode."
    (eval-when-compile
      (require 'racer))
    (unless (executable-find "cargo")
      (alert "Please install rust: https://www.rust-lang.org/en-US/install.html"))
    (unless (file-exists-p racer-cmd)
      (alert "rustup toolchain add nightly && cargo +nightly install racer"))
    (unless (file-exists-p racer-rust-src-path)
      (alert "rustup component add rust-src"))
    (racer-mode 1))
  :defer t
  :hook ((rust-mode . apm-racer-mode-setup)))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((css-mode html-mode) . rainbow-mode))

(use-package recentf
  :config (recentf-mode 1))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package ripgrep
  ;; for projectile to search with rg
  :ensure t
  :ensure-system-package (rg . ripgrep))

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1)
  ;; I wonder if this is causing large CPU usage like
  ;; https://github.com/syl20bnr/spacemacs/issues/9409
  :disabled t)

(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package scratch
  :ensure t
  :defer t)

(use-package selectrum
  :ensure t
  :disabled t
  :bind (("C-x C-z" . #'selectrum-repeat))
  :config (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
  :disabled t
  :config (selectrum-prescient-mode 1))

(use-package server
  :config
  ;; start emacs server only it has not already been started
  (unless (server-running-p)
    (server-start))
  (add-hook 'after-make-frame-functions
            #'(lambda (frame) (select-frame-set-input-focus frame)) t))

(use-package session-manager
  :load-path "vendor/"
  :config (session-manager-init "apm"))

(use-package sh-script
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

(use-package simple
  :defer t
  :diminish visual-line-mode
  :hook ((mu4e-view-mode . visual-line-mode))
  :init
  ;; save whatever is in the system clipboard to the kill ring before
  ;; killing something else into the kill ring
  (setq save-interprogram-paste-before-kill t)
  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow)))

(use-package smiley
  :custom (smiley-style 'emoji))

(use-package smtpmail
  ;; store password using secret-tool as follows:
  ;; secret-tool store --label='Canonical SMTP' host smtp.canonical.com port 587 user canonical
  ;; then enter PASSWORD
  :config
  (setq smtpmail-smtp-user "canonical")
  (setq smtpmail-smtp-server "smtp.canonical.com")
  (setq smtpmail-smtp-service 587))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package solarized-theme
  :ensure t
  :disabled t
  :config
  ;; settings for moody package
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  (setq x-underline-at-descent-line t)
  (setq solarized-distinct-fringe-background t)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-light t))

(use-package strace-mode
  :ensure t)

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package systemd
  :ensure t)

(use-package time
  :bind ((("C-c z" . world-clock)))
  :config (let ((team '(("Australia/Adelaide" . ("amurray"))
                        ("Europe/Brussels" . ("ebarretto"))
                        ("Europe/London" . ("ChrisCoulson"))
                        ("Brazil/East" . ("deafpool" "leosilva" "pfsmorigo"))
                        ("America/Argentina/Buenos_Aires" . ("emitorino"))
                        ("US/Eastern" . ("cipherboy" "dan" "shoonmcgregor" "sid" "vineetha1"))
                        ("Canada/Eastern" . ("Avital" "mdeslaur"))
                        ("US/Pacific" . ("jj" "sarnold" "sbeattie")))))
            ;; validate team
            (dolist (member team)
              (unless (file-exists-p (expand-file-name (car member) "/usr/share/zoneinfo"))
                (user-error "TZ %s does not exist!" (car member))))
            (require 'erc-hl-nicks)
            (setq zoneinfo-style-world-list
                    ;; make nicks stand out
                    (mapcar #'(lambda (member)
                                (list (car member)
                                      (string-join (cdr member) ", ")))
                            team)))
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (display-time))

(use-package tramp
  :ensure t
  :config (setq-default tramp-default-method "ssh"))

(use-package uct
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/"
  :demand t
  :bind (("C-c u u" . uct)
         ("C-c f c" . uct-find-cve)
         ("C-c u k" . uct-kernel-signoff-at-point)))

(use-package udev-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package usn
  :load-path "~/ubuntu/git/usn-tool/"
  :commands (usn-compose-from-draft)
  :bind (("C-c f u" . usn-show-at-point)))

(use-package uvt
  :load-path "~/ubuntu/git/ubuntu-qa-tools/vm-tools/"
  :bind (("C-c v" . uvt)))

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.php\\'" . web-mode))

(use-package vdiff
  :ensure t
  :config (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map))

(use-package vdiff-magit
  :ensure t
  :bind (:map magit-mode-map
              ("e" . vdiff-magit-dwim)
              ("E" . vdiff-magit))
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)))

(use-package vimrc-mode
  :ensure t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package vterm
  :load-path "/snap/emacs-vterm/current/"
  :bind (("C-x m" . vterm)))

(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region))
  :demand t
  :config
  (setq webpaste-paste-confirmation t)
  (add-to-list 'webpaste-providers-alist
               `("pastebin.ubuntu.com"
                 :uri "https://pastebin.ubuntu.com/"
                 ;; poster cannot be empty string and expiry in 1 day
                 :post-data (("poster" . ,user-full-name)
                             ("expiration" . "day"))
                 :post-field "content"
                 :post-lang-field-name "syntax"
                 ;; should this be common-lisp perhaps?
                 :lang-overrides ((emacs-lisp-mode . "clojure"))
                 :success-lambda webpaste--providers-success-response-url))
  (setq webpaste-provider-priority '("pastebin.ubuntu.com")))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))

(use-package whitespace
  :diminish whitespace-mode global-whitespace-mode
  :init (setq-default whitespace-style
                      '(face tabs tab-mark trailing))
  :config
  ;; whitespace-mode is not useful for erc or magit-log buffers
  (setq whitespace-global-modes '(not erc-mode magit-log-mode vterm-mode))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode 1))

(use-package world-time-mode
  :ensure t)

(use-package wttrin
  :load-path "~/git/emacs-wttrin/"
  :bind (("C-c f f" . wttrin))
  :config
  (setq wttrin-api-version 2)
  (setq wttrin-default-cities '("Adelaide")))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish (yas-global-mode yas-minor-mode)
  :config (yas-global-mode 1))

(use-package x86-lookup
  :ensure t
  :init (unless (file-exists-p "~/Documents/325383-sdm-vol-2abcd.pdf")
          (alert "Downloading Intel x86 instruction set to ~/Documents")
          (with-demoted-errors "Error downloading x86-lookup document: %s"
            (url-copy-file
             "https://software.intel.com/sites/default/files/managed/a4/60/325383-sdm-vol-2abcd.pdf"
             (expand-file-name "~/Documents/325383-sdm-vol-2abcd.pdf"))))
  :config (setq x86-lookup-pdf "~/Documents/325383-sdm-vol-2abcd.pdf")
  :bind ("C-h x" . x86-lookup))

(use-package xref
  :ensure t)

(use-package ztree
  :ensure t
  :bind (("C-x C-d" . ztree-dir))
  :config
  (setq ztree-draw-unicode-lines t)
  (setq ztree-show-number-of-children t))

(provide 'init)

;;; init.el ends here

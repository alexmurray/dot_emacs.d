;;; init.el --- Starting point for Alex Murray's Emacs Configuration -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; prefer newer non-byte compiled sources to older byte compiled ones
(setq load-prefer-newer t)

;;; Package management
(require 'package)

;; must be set before loading use-package
(defvar use-package-enable-imenu-support t)
(setq use-package-enable-imenu-support t)
;; compute stats info - needs to be set before loading use-package as well - see
;; stats with `use-package-report'
;;(setq use-package-compute-statistics t)
;; uncomment to debug package loading times
;; (setq use-package-verbose t)

(eval-and-compile
  (require 'use-package))

(use-package package
  :custom (package-install-upgrade-built-in t)
  :config
  ;; add melpa archive and gnu-devel
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))

  ;; use gnu over non-gnu over melpa over gnu-devel
  (setq package-archive-priorities '(("gnu" . 4) ("nongnu" . 2) ("melpa" . 1) ("gnu-devel" . 0)))

  ;; but use erc from gnu-devel
  (setq package-pinned-packages '((erc . "gnu-devel"))))

(use-package gnu-elpa-keyring-update
  :ensure t)

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

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)


;; customisations
(setq custom-file (locate-user-emacs-file "custom.el"))
;; load custom but ignore error if doesn't exist
(load custom-file 'noerror 'nomessage)

(defvar apm-preferred-dark-theme nil)
(defvar apm-preferred-light-theme nil)
(defvar apm-preferred-theme nil)

(defvar apm-load-preferred-theme-hook nil)

(defun apm-load-preferred-theme (dark)
  "Set the preferred DARK or light theme."
  (let ((theme (if (or (and (integerp dark) (= dark 1))
                       (and (stringp dark) (string= dark "prefer-dark")))
                   apm-preferred-dark-theme
                 apm-preferred-light-theme)))
    (when (and theme (not (eq theme apm-preferred-theme)))
      (when apm-preferred-theme
        (disable-theme apm-preferred-theme))
      (setq apm-preferred-theme theme)
      (load-theme apm-preferred-theme t)
      (run-hooks 'apm-load-preferred-theme-hook))))

(defun apm-desktop-portal-settings-changed (path var value)
  "Update preferred theme based on VALUE of VAR at PATH."
  (when (and (or (string-equal path "org.freedesktop.appearance")
                 (string-equal path "org.gnome.desktop.interface"))
             (string-equal var "color-scheme"))
    (apm-load-preferred-theme (car value))))

(defun apm-set-preferred-theme ()
  "Set preferred theme based on desktop color-scheme."
  (dbus-call-method-asynchronously
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "Read"
   (lambda (value)
     (apm-load-preferred-theme (caar value)))
   "org.freedesktop.appearance"
   "color-scheme"))

(when (require 'dbus nil t)
  ;; monitor for changes to the desktop portal settings
  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'apm-desktop-portal-settings-changed))

(use-package batppuccin
  :ensure t
  :demand t
  :preface
  (defun apm-batppuccin-setup ()
    "Set up batppuccin theme."
    (batppuccin-with-colors
      (set-face-attribute 'which-func nil :foreground bat-blue)))
  :hook ((batppuccin-after-load . apm-batppuccin-setup))
  :config
  (setq apm-preferred-dark-theme 'batppuccin-mocha)
  (setq apm-preferred-light-theme 'batppuccin-latte)
  (apm-set-preferred-theme))

(use-package alert
  :ensure t
  :config (when (eq system-type 'gnu/linux)
            (setq alert-default-style 'notifications)))

;;; General settings etc from C source so associate settings with emacs itself
(use-package emacs
  :preface
  ;; used in some of my yasnippet snippets
  (defun apm-camelize (s &optional delim)
    "Convert under_score string S to CamelCase string with optional DELIM."
    (interactive "s")
    (mapconcat 'identity (mapcar
                          #'(lambda (word) (capitalize (downcase word)))
                          (split-string s (if delim delim "_"))) ""))
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

  (defvar apm-gc-idle-timer nil)
  (defvar apm-gc-cons-threshold (* 200 1024 1024))
  (defvar apm-gc-idle-timeout 3.0)

  (defun apm-minibuffer-setup-hook ()
    "Set gc-cons-threshold to maximum when minibuffer is active."
    (setq gc-cons-threshold most-positive-fixnum))

  (defun apm-minibuffer-exit-hook ()
    "Reset gc-cons-threshold."
    (setq gc-cons-threshold apm-gc-cons-threshold))
  :hook
  ((minibuffer-setup . apm-minibuffer-setup-hook)
   (minibuffer-exit . apm-minibuffer-exit-hook))

  :config
  ;; use pipes for subprocess communication
  (setq-default process-connection-type nil)
  (setq gc-cons-threshold apm-gc-cons-threshold)
  (setq apm-gc-idle-timer
        (run-with-idle-timer
         apm-gc-idle-timeout
         t
         #'garbage-collect))
  ;; display messages during garbage collection
  (setq garbage-collection-messages t)

  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  ;; personalisation
  (setq user-full-name "Alex Murray")
  (setq user-mail-address "murray.alex@gmail.com")

  ;; enable narrow-to-region
  (put 'narrow-to-region 'disabled nil)

  ;; tabs are never ok
  (setq-default indent-tabs-mode nil)

  ;; set a reasonable fill and comment column
  (setq-default fill-column 80)
  (setq-default comment-column 80)

  ;; don't try and use dialog boxes
  (setq-default use-dialog-box nil)
  (setq-default use-file-dialog nil)

  ;; inhibit startup message and splash screen
  (setq inhibit-startup-message t)
  ;; remove message from initial scratch buffer
  (setq initial-scratch-message nil)

  ;; don't restore window layout on minibuffer exit
  (setq read-minibuffer-restore-windows nil)

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

  ;; system font doesn't seem to scale properly in emacs so set it manually
  (let ((preferred-font "Ubuntu Sans Mono-11"))
    (if (daemonp)
        (add-to-list 'default-frame-alist `(font . ,preferred-font))
      (if (null (font-info preferred-font))
          (alert "Please apt install fonts-ubuntu")
        ;; apply this to all existing and future frames
        (set-frame-font preferred-font nil t))))

  ;; make emacs less laggy
  (setq inhibit-compacting-font-caches t)

  (set-language-environment "UTF-8")

  ;; prompt when trying to switch out of a dedicated window
  (setq switch-to-buffer-in-dedicated-window 'prompt)

  ;; ensure scrolling forwards / backwards preserves original location such that
  ;; they undo each other
  (setq scroll-preserve-screen-position 'always)

  (bind-key [remap fill-paragraph] #'endless/fill-or-unfill)

  ;; case insensitive completion everywhere
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)

  ;; show empty lines
  (setq indicate-empty-lines t)

  ;; allow recursive minibuffer usage
  (setq enable-recursive-minibuffers t))

;;; Packages
(use-package abbrev
  :config
  (setq save-abbrevs t)
  (setq-default abbrev-mode t))

(use-package adaptive-wrap
  :ensure t
  :defer t
  :hook ((text-mode . adaptive-wrap-prefix-mode)))

(use-package agent-shell
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :defer t
  :config (global-aggressive-indent-mode))

(use-package all-the-icons
  :ensure t
  :config
  (unless (file-exists-p (expand-file-name "~/.local/share/fonts/all-the-icons.ttf"))
    (alert "Installing all-the-icons font...")
    (all-the-icons-install-fonts)))

(use-package all-the-icons-completion
  :ensure t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :hook (after-init . all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)))

(use-package android-mode
  :ensure t)

(use-package ansi-color
  :hook ((compilation-filter . ansi-color-compilation-filter))
  ;; show colours correctly in shell
  :config (ansi-color-for-comint-mode-on))

(use-package apheleia
  :ensure t
  :config
  ;; use ruff for python formatting
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist)
          '(ruff-isort ruff)))
  :config (apheleia-global-mode 1))

(use-package apm-misc
  :load-path "lisp/"
  :disabled t
  :bind (("C-c b l" . apm-browse-lp-bug-at-point)))

(use-package apparmor-mode
  :load-path "~/git/apparmor-mode/"
  :config
  (add-to-list 'auto-mode-alist
               `(,(expand-file-name "~/git/apparmor/profiles/apparmor.d/") . apparmor-mode))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(apparmor-mode . ("apparmor-language-server")))))

(use-package apropos
  :bind ("C-h a" . apropos))

(use-package arc-mode
  :config  (add-to-list 'auto-mode-alist '("\\.snap\\'" . archive-mode)))

(use-package asn1-mode
  :ensure t
  :defer t
  :mode (("\\.asn1?$" . asn1-mode)))

(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1))

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

(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode 1))

(use-package auth-source
  ;; prefer encrypted auth source to non-encrypted
  :init
  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))

(use-package beginend
  :ensure t
  :config
  ;; beginend defines lots of different modes so diminish them all
  (dolist (m beginend-modes)
    (diminish (cdr m)))
  (beginend-global-mode 1))

(use-package blamer
  :ensure t
  :custom
  ;; seem to get errors if trying to blame the selected lien as well so only show for the current line aka visual
  (blamer-type 'visual)
  (blamer-commit-formatter "· %s")
  (blamer-idle-time 0.2)
  :config
  (global-blamer-mode 1)
  (with-eval-after-load 'git-commit
    (setq blamer-max-commit-message-length git-commit-summary-max-length)))

(use-package breadcrumb
  :ensure t
  :config (breadcrumb-mode 1))

(use-package browse-kill-ring
  :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package browse-url
  ;; since using the firefox snap this needs to be somewhere that a snap
  ;; can access and global /tmp is not that... plus this also means we
  ;; don't drop our random files in the global /tmp which is good too
  :custom ((browse-url-temp-dir (expand-file-name "~/tmp"))))

(use-package bs
  :bind ("C-x C-b" . bs-show))

(use-package calc
  :defer t
  ;; https://emacs.ch/@galdor/109759892132784831
  :custom (calc-multiplication-has-precedence nil))

(use-package calendar
  :custom (calendar-week-start-day 1)
  :config
  ;; show ISO week numbers in calendar
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 1.0 :foreground "salmon")
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))

  (copy-face 'default 'calendar-iso-week-header-face)
  (set-face-attribute 'calendar-iso-week-header-face nil
                      :height 1.0 :foreground "salmon")
  (setq calendar-intermonth-header
        (propertize "Wk"
                    'font-lock-face 'calendar-iso-week-header-face)))

(use-package cargo
  :ensure t
  :init (unless (executable-find "cargo")
          (alert "Please install the rustup snap"))
  :defer t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package cc-mode
  :defer t
  :custom (c-basic-offset 2))

(use-package c-ts-mode
  :config
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  :custom (c-ts-mode-indent-offset 2)
  (c-ts-mode-indent-style 'linux))

(use-package comint
  :hook ((comint-output-filter-functions . comint-osc-process-output)))

(use-package comint-mime
  :ensure t
  :hook ((shell-mode . comint-mime-setup)))

(use-package copy-as-format
  :ensure t
  ;; add bindings for github, gitlab, jira, markdown, org-mode, and slack (aka
  ;; mattermost)
  :bind (("C-c w w" . copy-as-format)
         ("C-c w g" . copy-as-format-github)
         ("C-c w l" . copy-as-format-gitlab)
         ("C-c w j" . copy-as-format-jira)
         ("C-c w m" . copy-as-format-markdown)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w s" . copy-as-format-slack)))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  :init
  (global-corfu-mode 1)
  (setq tab-always-indent 'complete)
  ;; cycle with at least 3 candidates
  (setq completion-cycle-threshold 3))

(use-package corfu-popupinfo
  :ensure corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :bind (:map corfu-map
              ("M-p" . corfu-popupinfo-scroll-down)
              ("M-n" . corfu-popupinfo-scroll-up)
              ("M-d" . corfu-popupinfo-toggle)))

(use-package cape
  :ensure t
  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package compile
  :hook ((shell-mode . compilation-shell-minor-mode))
  :bind ("<f5>" . compile)
  ;; automatically scroll to first error on output
  :config (setq compilation-scroll-output 'first-error)
  ;; use compilation-mode for _source.build files
  (add-to-list 'auto-mode-alist '("_\\(amd64\\|source\\)\\.build\\'" . compilation-mode)))

(use-package completion-preview
  :init (global-completion-preview-mode 1)
  :disabled t ; doesn't yet play well with corfu
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate)
              ("M-i" . completion-preview-insert))
  :custom (completion-preview-minimum-symbol-length 2)
  :config
  ;; Org mode has a custom `self-insert-command'
  (push 'org-self-insert-command completion-preview-commands))

(use-package consult
  :ensure t
  :bind (
         ("C-c h" . consult-history)
         ("C-c s" . consult-clock-in)
         ("C-x b" . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-i" . consult-imenu)
         ("M-s i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s o" . consult-outline)
         ("C-y". consult-yank-pop)
         ("M-y". consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         ;; https://emacsredux.com/blog/2021/11/25/redo-complex-command-with-consult/
         ([remap repeat-complex-command] . consult-complex-command)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line)
         :map minibuffer-mode-map
         ("C-y" . yank))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; from https://github.com/minad/consult/wiki#org-clock with a minor fix to
  ;; replace 'consult--candidate text property with 'org-marker since it seems
  ;; to be broken for me as provided upstream
  (defun consult-clock-in (&optional match scope resolve)
    "Clock into an Org heading."
    (interactive (list nil nil current-prefix-arg))
    (require 'org-clock)
    (org-clock-load)
    (save-window-excursion
      (consult-org-heading
       match
       (or scope
           (thread-last org-clock-history
                        (mapcar 'marker-buffer)
                        (mapcar 'buffer-file-name)
                        (delete-dups)
                        (delq nil))
           (user-error "No recent clocked tasks")))
      (org-clock-in nil (when resolve
                          (org-resolve-clocks)
                          (org-read-date t t)))))

  (consult-customize consult-clock-in
                     :prompt "Clock in: "
                     :preview-key "M-."
                     :group
                     (lambda (cand transform)
                       (let* (;; (marker (get-text-property 0 'consult--candidate cand))
                              (marker (get-text-property 0 'org-marker cand))
                              (name (if (member marker org-clock-history)
                                        "*Recent*"
                                      (buffer-name (marker-buffer marker)))))
                         (if transform (substring cand (1+ (length name))) name)))))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-s i" . consult-eglot-symbols)))

(use-package consult-xref-stack
  :vc (:url "https://github.com/brett-lempereur/consult-xref-stack/")
  :bind (("C-," . consult-xref-stack-backward)))

(use-package copilot
  :ensure t
  :demand t
  :preface
  (defun apm-copilot-complete-or-accept ()
    (interactive)
    (if (copilot--overlay-visible)
        (copilot-accept-completion)
      (copilot-complete)))
  :bind (:map copilot-mode-map
              ;; enable binding is normal copilot map as doing it just in the
              ;; completion map doesn't seem sufficient and gets overridden
              ;; sometimes (and hence they don't work)
              ("C-<tab>" . apm-copilot-complete-or-accept)
              ("C-TAB" . apm-copilot-complete-or-accept)
              ("C-<backtab>" . copilot-accept-completion-by-word)
              ("C-<iso-lefttab>" . copilot-accept-completion-by-word)
              ("C-<end>" . copilot-accept-completion-by-line)
              :map copilot-completion-map
              ("C-<tab>" . apm-copilot-complete-or-accept)
              ("C-TAB" . apm-copilot-complete-or-accept)
              ("C-<backtab>" . copilot-accept-completion-by-word)
              ("C-<iso-lefttab>" . copilot-accept-completion-by-word)
              ("C-<end>" . copilot-accept-completion-by-line)
              ("M-n" . copilot-next-completion)
              ("M-p" . copilot-previous-completion))
  :hook ((prog-mode . copilot-mode))
  :custom (copilot-server-executable "/snap/bin/copilot-language-server"))

(use-package cov
  :ensure t
  :hook ((prog-mode . cov-mode)))

(use-package crontab-mode
  :ensure t)

(use-package cve-mode
  :load-path "~/ubuntu/git/ubuntu-cve-tracker/scripts/")

;; show suspicious c constructs automatically
(use-package cwarn
  :init (global-cwarn-mode 1))

(use-package cycle-at-point
  :ensure t
  :bind (("M-p" . cycle-at-point)))

(use-package deadgrep
  :ensure t
  :init (unless (executable-find "rg")
          (alert "Please apt install ripgrep")))

(use-package devhelp
  :ensure t)

(use-package diff-mode
  :mode (("\\.debdiff\\'" . diff-mode)))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-vc-rename-file t)
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package disk-usage
  :ensure t)

(use-package display-fill-column-indicator
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode)))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode))
  :custom
  (display-line-numbers-widen t))

(use-package disproject
  :ensure t
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind (:map ctl-x-map
              ("p" . disproject-dispatch)))

(use-package dpkg-dev-el
  :init (unless (executable-find "debputy")
          (alert "Please apt install dh-debputy"))
  :ensure t)

(use-package debian-autopkgtest-control-mode
  :ensure dpkg-dev-el
  :hook ((debian-autopkgtest-control-mode . eglot-ensure))
  :config (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '(debian-autopkgtest-control-mode . ("debputy" "lsp" "server")))))

(use-package debian-changelog-mode
  :ensure dpkg-dev-el
  :hook ((debian-changelog-mode . eglot-ensure))
  :config (let ((pockets '("" "-updates" "-security"))
                (releases (delete-dups (append (split-string
                                                (shell-command-to-string
                                                 "distro-info --supported"))
                                               (split-string
                                                (shell-command-to-string
                                                 "distro-info --supported-esm"))
                                               (split-string
                                                (shell-command-to-string
                                                 "distro-info --devel"))))))
            (dolist (release releases)
              (dolist (pocket pockets)
                (add-to-list 'debian-changelog-allowed-distributions (concat release pocket)))))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(debian-changelog-mode . ("debputy" "lsp" "server")))))

(use-package debian-control-mode
  :ensure dpkg-dev-el
  :hook ((debian-control-mode . eglot-ensure))
  :config (with-eval-after-load 'eglot
            (add-to-list 'eglot-server-programs '(debian-control-mode . ("debputy" "lsp" "server")))))

(use-package delsel
  ;; enable delete-selection mode to allow replacing selected region
  ;; with new text automatically
  :init (delete-selection-mode 1))

(use-package diff-hl
  :ensure t
  ;; Integrate with Magit and highlight changed files in the fringe of dired
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config (global-diff-hl-mode 1))

(use-package diffview
  :ensure t)

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
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally))

(use-package eglot
  :hook ((prog-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :bind (:map eglot-mode-map ("<f2>" . eglot-rename))
  :custom
  (eglot-extend-to-xref t)
  (eglot-documentation-strategy 'eldoc-documentation-compose-eagerly)
  ;; improve performance by logging less
  (eglot-events-buffer-config '(:size 20000 :format short))
  :custom-face (eglot-highlight-symbol-face ((t (:inherit bold :underline t))))
  :config
  ;; speed up performance
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(cmake-ts-mode "neocmakelsp" "stdio"))
  ;; enable formatting in vscode-json-languageserver
  (add-to-list 'eglot-server-programs '(json-mode "vscode-json-languageserver" "--stdio"
                                                  :initializationOptions (:provideFormatter t)))
  (add-to-list 'eglot-server-programs '(markdown-mode "vscode-markdown-languageserver" "--stdio"
                                                      :initializationOptions (:markdownFileExtensions ["md"])))
  ;; use vale-lsp over markdown-languageserver as the former still doesn't work
  ;; as a snap...
  (add-to-list 'eglot-server-programs '(markdown-mode "vale.vale-ls"))
  (add-to-list 'eglot-server-programs `(vimrc-mode "vim-language-server" "--stdio"
                                                   :initializationOptions (:vim-runtime ,(car (file-expand-wildcards "/usr/share/vim/vim*"))))))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster/")
  :disabled t
  :after eglot
  :init (unless (executable-find "emacs-lsp-booster")
          (alert "Please install emacs-lsp-booster to ~/bin from https://github.com/blahgeek/emacs-lsp-booster/releases"))
  :config (eglot-booster-mode))

(use-package eglot-inactive-regions
  :ensure t
  :after eglot
  :config (eglot-inactive-regions-mode 1))

(use-package eldoc
  :config (global-eldoc-mode 1))

(use-package eldoc-box
  :ensure t
  ;; tie in with eglot
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)))

(use-package eldoc-diffstat
  :ensure t
  :config (global-eldoc-diffstat-mode 1))

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
  :hook ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(use-package elisp-mode
  ;; ensure flymake-mode is off for elisp files by default to minimise the
  ;; chance of RCE -
  ;; https://eshelyaron.com/posts/2024-11-27-emacs-aritrary-code-execution-and-how-to-avoid-it.html
  :hook (emacs-lisp-mode . flymake-mode-off))

(use-package epg
  :config
  (setq epg-user-id "murray.alex@gmail.com"))

(use-package erc
  :pin gnu-devel
  :ensure t
  :preface
  (eval-when-compile
    (require 'erc-log)
    (require 'erc-match))

  (defun apm-prompt-to-connect-to-irc ()
    "Prompt to connect to irc."
    (interactive)
    (let ((connectivity (string-trim
                         (shell-command-to-string "nmcli networking connectivity")))
          (nick "amurray")
          (server "192.168.1.228")
          (port "7076"))
      (if (string= connectivity "full")
          (when (y-or-n-p "Connect to IRC? ")
            ;; connect to matterircd on localhost and oftc and freenode via znc
            ;;(erc :server "localhost" :port "6667" :nick "alexmurray")
            (erc-tls :server server :port port
                     :nick nick :password (concat nick "/OFTC:"
                                                  (auth-source-pick-first-password
                                                   :user nick
                                                   :host server
                                                   :port port)))
            (erc-tls :server server :port port
                     :nick nick :password (concat nick "/libera:"
                                                  (auth-source-pick-first-password
                                                   :user nick
                                                   :host server
                                                   :port port))))
        (message "Network connectivity is %s, not prompting to connect to IRC" connectivity))))

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
              ("M-s e" . apm-occur-in-erc)
              :map erc-fill-wrap-mode-map
              ("C-c a" . org-agenda))
  :config
  (eval-and-compile
    (require 'erc-button)
    (require 'erc-desktop-notifications)
    (require 'erc-fill)
    (require 'erc-join)
    (require 'erc-log)
    (require 'erc-match)
    (require 'erc-nicks)
    (require 'erc-networks)
    (require 'erc-notify)
    (require 'erc-services)
    (require 'erc-track))

  (setq erc-user-full-name user-full-name)
  (setq erc-nick (list user-login-name "alexmurray"))

  ;; make prompt more dynamic
  (setq erc-prompt #'erc-prompt-format)
  (setq erc-prompt-for-nickserv-password nil)

  (setq erc-use-auth-source-for-nickserv-password t)

  (setq erc-autojoin-timing 'ident)

  ;; since we connect to oftc directly, we need to autojoin channels there
  ;; - not needed for libera (since we use ZNC)
  (setq erc-autojoin-channels-alist nil)
  (setq erc-fill-function #'erc-fill-wrap)
  ;; account for really long names
  (setq erc-fill-static-center 22)
  ;; this fits on a dual horizontal split on my laptop
  (setq erc-fill-column 110)

  ;; use sensible buffer names with server as well
  (setq erc-rename-buffers t)

  ;; try harder to reconnect but wait longer each time since it may take a
  ;; while to get a DHCP lease etc
  (setq erc-server-reconnect-function #'erc-server-delayed-check-reconnect)
  (setq erc-server-auto-reconnect t)

  (setq erc-scrolltobottom-all t)

  (add-to-list 'erc-modules 'button)
  (add-to-list 'erc-modules 'log)
  (add-to-list 'erc-modules 'match)
  (add-to-list 'erc-modules 'nicks)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'scrolltobottom)
  (add-to-list 'erc-modules 'services)
  (add-to-list 'erc-modules 'services-regain)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)

  ;; format nicknames to show if user has voice(+), owner (~), admin (&),
  ;; operator (@)
  (setq erc-show-speaker-membership-status t)

  (setq erc-keywords '("alexmurray" "cve" "vulnerability" "apparmor" "seccomp" "exploit" "security" "esm" "@here" "@all" "@channel" "@security"))

  ;; when joining don't bring to front
  (setq erc-join-buffer 'bury)

  ;; ensure erc-track plays nicer with minions - https://github.com/tarsius/minions/issues/22
  (setq erc-track-position-in-mode-line t)
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
  (setq erc-track-exclude '("#emacs" "#emacsconf" "#ubuntu"))
  (setq erc-track-shorten-function nil)

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

  ;; make sure we identify to nickserv
  (erc-services-mode 1)

  ;; change header line face when disconnected
  (setq erc-header-line-face-method
        #'apm-erc-update-header-line-show-disconnected)

  ;; make sure any privmsg (which is via query buffers) show up as urgent
  ;; in track list
  (defadvice erc-track-select-mode-line-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
        (setq ad-return-value 'erc-current-nick-face)
      ad-do-it)))

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
          (shell-command (concat  "tic " (car (file-expand-wildcards "/snap/emacs/current/usr/share/emacs/*/etc/e/eterm-color.ti"))))))

(use-package executable
  :hook ((after-save . executable-make-buffer-file-executable-if-script-p)))

(use-package expreg
  :ensure t
  :bind (("C-'" . expreg-expand)
         ("C-\"" . expreg-contract)))

(use-package files
  :bind (("C-c r b" . revert-buffer))
  :config
  :custom
  (view-read-only t)
  (save-some-buffers-default-predicate #'save-some-buffers-root))

(use-package flash
  :ensure t
  :bind (("s-j" . flash-jump)))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom (flymake-mode-line-lighter "ℹ")
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package flymake-codespell
  :ensure t
  :init (unless (executable-find "codespell")
          (alert "Please apt install codespell"))
  :hook (prog-mode . flymake-codespell-setup-backend))

(use-package flymake-ruff
  :ensure t
  :init (unless (executable-find "ruff")
          (alert "Please snap install ruff"))
  :preface (defun apm-flymake-ruff-load ()
             (when (and (derived-mode-p 'python-base-mode)
                        (not (eq buffer-file-name nil)))
               (flymake-ruff-load)))
  ;; load via eglot - but this should only be done in python-mode buffers
  :hook (eglot-managed-mode . apm-flymake-ruff-load)
  ;; bypass snap confinement since apparmor blocks snap-confine and hence the ruff snap from inheriting the file-descriptor for the temp file created by call-process-region used internally by flymake-ruff
  :custom (flymake-ruff-program "/snap/ruff/current/bin/ruff"))

(use-package forge
  :ensure t
  :after magit)

(use-package forge-launchpad
  :load-path "vendor/"
  :after forge)

(use-package fringe
  :custom (indicate-empty-line t)
  ;; ensure we indicate empty lines via fringe-mode with defaults
  :config (fringe-mode))

(use-package fuzzy
  :ensure t)

(use-package gdb-mi
  :defer t
  :init
  ;; use gdb-many-windows by default
  (setq-default gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  (setq-default gdb-show-main t))

(use-package gh-notify
  :ensure t)

(use-package gitattributes-mode
  :ensure git-modes
  :defer t)

(use-package git-commit-mode
  :ensure magit
  :after magit
  :preface (defun apm-git-commit-mode-setup ()
             (setq-local fill-column 75))
  :hook ((git-commit-mode . apm-git-commit-mode-setup))
  :config (git-commit-turn-on-auto-fill))

(use-package gitconfig-mode
  :ensure git-modes
  :defer t)

(use-package gitignore-mode
  :ensure git-modes
  :defer t)

(use-package gnuplot
  :ensure t)

(use-package gnu-elpa
  :defer t
  :ensure t)

(use-package goggles
  :ensure t
  :config (goggles-mode 1))

(use-package go-mode
  :ensure t
  ;; plug go-ts-mode into gofmt
  :preface (defun apm-gofmt-before-save ()
             (interactive)
             (when (member major-mode '(go-mode go-ts-mode))
               (gofmt)))
  :hook ((before-save . apm-gofmt-before-save)))

(use-package gotest
  :ensure t
  ;; snapd uses the check.v1 package for defining test suites rather than
  ;; testify so we can't specify a single test using -testify.m - instead this
  ;; needs to be -check.f - but gotest hardcodes the use of testify so
  ;; unfortunately we can't customise this - instead just ensure than whenever
  ;; go-test--go-test is called that we replace any instance of -testify.m with
  ;; -check.f
  :config (define-advice go-test--go-test (:filter-args (args) apm-go-test--go-test)
            ;; args is a list containing the arguments passed to
            ;; go-test--go-test - which is the command-line arguments as a
            ;; single string and an optional env which we need to retain
            (append (list (replace-regexp-in-string "-testify\.m" "-check\.f" (car args)))
                    (cdr args))))

(use-package goto-addr
  :defer t
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)))

(use-package goto-line-preview
  :ensure t
  :config (global-set-key [remap goto-line] 'goto-line-preview))

(use-package gptel
  :ensure t
  ;; configure to use local ollama instance
  :config (setq
           gptel-model 'qwen3.5:4b
           gptel-backend (gptel-make-ollama "Ollama"
                           :host "localhost:11434"
                           :stream t
                           :models '(qwen3.5:4b))))

(use-package gud
  :defer t
  :hook ((gud-mode . gud-tooltip-mode)))

(use-package help-at-pt
  :custom (help-at-pt-display-when-idle t))

(use-package help-fns
  :config (setq describe-bindings-outline t))

(use-package helpful
  :ensure t
  :bind (([remap describe-key]      . helpful-key)
         ([remap describe-command]  . helpful-command)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-function] . helpful-callable)))

(use-package hideshow
  ;; use hs-minor-mode in programming and mail composing - TODO - get it
  ;; working during mail viewing as well to be able to hide quoted bits
  ;; - something like:
  :hook ((prog-mode message-mode) . hs-minor-mode))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-printf
  :ensure t
  :hook ((prog-mode . hl-printf-mode)))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  ;; add flymake backend too
  (when (fboundp 'flymake-hl-todo)
    ;; TODO - apparently should use depth nil and set 'local as well...
    (add-hook 'flymake-diagnostic-functions #'flymake-hl-todo)))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode 1))

(use-package imenu
  :bind (("M-i" . imenu)))

(use-package keypression
  :ensure t)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package jinx
  :ensure t
  :preface
  ;; jinx uses emacs modules and we need to ensure it compiles with gcc from the
  ;; emacs snap
  (define-advice jinx--load-module (:around (orig-fun &rest args) apm-jinx--load-module)
    "Ensure that the module is compiled with the correct gcc."
    (let* ((sysroot (file-name-as-directory (concat (file-name-as-directory (getenv "EMACS_SNAP_USER_COMMON")) "sysroot")))
           (process-environment (append `(,(concat "CC=" sysroot "usr/bin/gcc-14" )
                                          ,(concat "PKG_CONFIG_PATH=" (car (file-expand-wildcards (concat sysroot "usr/lib/*/pkgconfig")))))
                                        process-environment))
           (jinx--compile-flags (append jinx--compile-flags
                                        (list (concat "--sysroot=" sysroot)
                                              (concat "-Wl,-rpath=" sysroot "/lib/x86_64-linux-gnu")))))
      (apply orig-fun args)))
  :bind (("M-$" . jinx-correct)
         :map jinx-mode-map ("C-;" . jinx-correct))
  :hook ((emacs-startup . global-jinx-mode)))

(use-package journalctl
  :ensure t
  :vc (:url "https://github.com/WJCFerguson/journalctl/"))

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

(use-package let-completion
  :ensure t
  :hook ((emacs-lisp-mode . let-completion-mode)))

(use-package lin
  :ensure t
  :config (setq lin-face 'lin-cyan-override-fg))

(use-package link-hint
  :ensure t
  :bind
  ("C-c C-l C-o" . link-hint-open-link)
  ("C-c C-l C-c" . link-hint-copy-link))

(use-package lxd-tramp
  :ensure t
  :preface (defun apm-recentf-ignore-lxd-tramp (file)
             "Do not keep FILE if it is on a lxd remote."
             ;; return non-nil to keep in recentf-list
             (not (string-match "^/lxd:.*" file)))
  :config (with-eval-after-load 'recentf
            (add-to-list 'recentf-keep 'apm-recentf-ignore-lxd-tramp)))

(use-package lp
  :load-path "/snap/gitlptools/current"
  :after magit
  :init (unless (executable-find "git-lp-open")
          (alert "Please install the gitlptools snap")))

(use-package magit
  :ensure t
  :custom ((magit-diff-refine-hunk t)
           (magit-format-file-function #'magit-format-file-all-the-icons))
  :bind (("C-x g" . magit-status))
  :demand t)

(use-package magit-patch-changelog
  :ensure t)

(use-package magit-popup
  ;; whilst magit doesn't need this anymore, other packages do and magit
  ;; doesn't provide it anymore so explicitly add it
  :ensure t)

(use-package marginalia
  :ensure t
  :hook ((emacs-startup . marginalia-mode)))

(use-package markdown-mode
  :ensure t
  :init (unless (executable-find "markdown")
          (alert "Please apt install discount"))
  ;; ensure it is loaded since is used by eglot
  :demand t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package mb-depth
  :config (minibuffer-depth-indicate-mode 1))

(use-package mermaid-mode
  :ensure t
  :defer t
  :custom
  ;; we are using the snap so make sure it has somewhere it can read to/write from by default rather than the global /tmp/
  ((mermaid-tmp-dir (expand-file-name "~/snap/mermaid-cli/common/"))
   ;; workaround bug in mermail.el where an empty mermaid-flags results in errors when spawning mmdc since it sees an empty command-line argument and complains - so set mermaid-flags to something relatively innocuous
   (mermaid-flags "-q")))

(use-package meson-mode
  :ensure t)

(use-package message
  ;; to better support format=flowed
  :hook (message-mode . use-hard-newlines)
  :custom
  ;; use standard completion UI for message completion
  (message-expand-name-standard-ui t)
  (message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:\n")
  (message-citation-line-function #'message-insert-formatted-citation-line)
  (message-make-forward-subject-function 'message-forward-subject-fwd)
  ;; kill message buffer after sending rather than burying
  (message-kill-buffer-on-exit t)
  ;; disable filling of long lines
  (message-fill-column nil))

(use-package minibuffer
  :config
  (setq completion-styles '(substring orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))))
  (setq completion-auto-help 'visible)
  (setq completion-auto-select 'second-tab))

(use-package minions
  :ensure t
  :config
  (add-to-list 'minions-prominent-modes 'flymake-mode)
  (minions-mode 1))

(use-package message-attachment-reminder
  :ensure t)

(use-package modern-cpp-font-lock
  :ensure t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)))

(use-package mouse
  :config (context-menu-mode 1))

(use-package move-text
  :ensure t
  :bind (([(meta shift up)] . move-text-up)
         ([(meta shift down)] . move-text-down)))

(use-package nano-agenda
  :ensure t)

(use-package nhexl-mode
  :ensure t)

(use-package notmuch
  :ensure t
  :init
  (unless (executable-find "notmuch")
    (alert "Please apt install notmuch"))
  (unless (executable-find "afew")
    (alert "Please apt install afew"))
  :preface
  ;; discourage the use of text/plain for certain senders
  (defvar apm-notmuch-discouraged-senders '((("text/plain") . ("forum@forum.snapcraft.io"
                                                               "noreply@discourse.ubuntu.com"
                                                               "noreply@discourse.canonical.com"
                                                               "bounce@websense.com"
                                                               "wsm-postmaster@intel.com"
                                                               "no-reply@onepointpay.com.au"))))
  (defun apm-notmuch-determine-discouraged (msg)
    "Determine is MSG wants text/plain to be discouraged."
    (let* ((headers (plist-get msg :headers))
           (from (or (plist-get headers :From) ""))
           (discouraged '("text/html" "multipart/related")))
      (dolist (discouraged-sender apm-notmuch-discouraged-senders)
        (dolist (sender (cdr discouraged-sender))
          (when (string-search sender from)
            (setq discouraged (car discouraged-sender)))))
      discouraged))

  (defun apm-notmuch-show-view-lp-build-log ()
    "Show the build log for the current message in a new buffer."
    (interactive)
    ;; find the build log URL in the current message, and open it in a new buffer
    ;; with compilation-mode to view the log
    (unless (eq major-mode 'notmuch-show-mode)
      (error "Not in notmuch-show-mode"))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Build Log:" nil t)
        (re-search-forward "https?://[^[:space:]]+" nil t)
        (when-let ((url (thing-at-point 'url)))
          (message "Fetching build log... %s" url)
          (condition-case err
              (let ((buffer (url-retrieve-synchronously url t)))
                (with-current-buffer buffer
                  (rename-buffer (format "*Build Log: %s*" url) t)
                  (compilation-mode)
                  (pop-to-buffer buffer)
                  ;; also run analyse-build-log when available to pinpoint the line
                  ;; of interest then scroll to that line
                  (if (executable-find "analyse-build-log")
                      (let ((line))
                        (with-temp-buffer
                          (insert-buffer-substring buffer)
                          ;; lpci logs add :: prefix to lines which confuses
                          ;; the analysis so remove this first
                          (save-excursion
                            (save-match-data
                              (goto-char (point-min))
                              (while (re-search-forward "^:: " nil t)
                                (replace-match ""))))
                          (message "Analysing build log...")
                          (shell-command-on-region (point-min) (point-max) "analyse-build-log /dev/stdin" t t)
                          (compilation-mode)
                          (save-excursion
                            (goto-char (point-min))
                            (when (re-search-forward "\\(Issue found at line\\|Failed line:\\) \\([0-9]+\\)" nil t)
                              (setq line (string-to-number (match-string 2)))
                              ;; show the full output to the user
                              (message (buffer-substring (point-min) (point-max))))))
                        (when line
                          (with-current-buffer buffer
                            (forward-line (1- line)))))
                    (message "analyse-build-log not found - install python3-buildlog-consultant or buildlog-consultant to support automatic error finding"))))
            (error (message "Failed to download build log or analyse it: %s" (cdr err))))))))
  :bind (("C-c m" . notmuch)
         :map notmuch-show-mode-map
         ("C-c C-l C-b" . apm-notmuch-show-view-lp-build-log))
  :custom
  (notmuch-wash-wrap-lines-length 150)
  (notmuch-print-mechanism #'notmuch-print-ps-print/evince)
  :config
  (eval-and-compile
    (require 'notmuch)
    (require 'notmuch-show)
    (require 'notmuch-tree))
  (setq notmuch-multipart/alternative-discouraged 'apm-notmuch-determine-discouraged)
  (defun apm-prompt-to-report-spam (subject url)
    (and (y-or-n-p (format "Do you also want to report this message \"%s\" as spam to mailcontrol? " subject))
         (url-retrieve (concat url)
                       (lambda (s)
                         (let ((status (url-http-symbol-value-in-buffer
                                        'url-http-response-status (current-buffer))))
                           (pcase status
                             (200 (message "Reported '%s' as spam" subject))
                             (_ (user-error "Failed to report as spam: %s" status))))) ))    )

  (defun apm-get-websense-blocklist-url ()
    "Get websense blocklist URL via the most recently received summary email."
    ;; the summary email is sent with subject "Personal Email Subscription - Forcepoint Email Security Cloud"
    (let ((summary-email (shell-command-to-string "notmuch show --include-html --sort=newest-first --limit 1  subject:\"Personal Email Subscription - Forcepoint Email Security Cloud\"")))
      (when (string-match "\\(https://\\(admin.websense.net\\|www.mailcontrol.com\\)/r/[^?]*\\).*Manage Allow/Block Lists" summary-email)
        (match-string 1 summary-email))))

  (defun apm-prompt-to-add-email-to-forcepoint-blocklist (email description)
    "Add EMAIL to the forcepoint blocklist with DESCRIPTION."
    (let* ((blocklist-url (or (apm-get-websense-blocklist-url)
                              (user-error "No URL found for managing websense blocklist")))
           (url (concat blocklist-url "?page=bw_add"))
           (n-similar (+ (string-to-number (shell-command-to-string (format "notmuch count from:%s" email)))
                         (string-to-number (shell-command-to-string (format "notmuch count from:%s and tag:spam" email)))))
           (response (if (> n-similar 1)
                         (cadr
                          (read-multiple-choice
                           (format "Add %s to the blocklist with description '%s' (%d total emails from this sender)? " email description n-similar)
                           '((?y "yes" "Yes - using the suggested description")
                             (?e "edit" "Yes - but using a different description")
                             (?n "no" "No - do not add to the blocklist"))
                           nil nil (and (not use-short-answers)
                                        (not (use-dialog-box-p)))))
                       (message "Only 1 email from %s so not prompting to add to blocklist" email))))
      (unless (equal response "no")
        (when (equal response "edit")
          (setq description (read-string "Description: " description)))
        (let ((url-request-method "POST")
              (url-request-extra-headers
               '(("Content-Type" . "application/x-www-form-urlencoded")))
              (url-request-data (concat "action=save&action_general=deny&"
                                        "email_1=" (url-encode-url email) "&"
                                        "description_1=" (url-encode-url description))))
          (url-retrieve url
                        (lambda (_)
                          (let ((status (url-http-symbol-value-in-buffer
                                         'url-http-response-status (current-buffer))))
                            (pcase status
                              (200 (message "Added %s to the blocklist with description '%s'" email description))
                              (_ (user-error "Failed to add %s to the blocklist" email))))))
          t))))
  ;; requires to have set the following in ~/.notmuch-config so that the X-MailControl-ReportSpam header is available
  ;;
  ;; [show]
  ;; extra_headers=X-MailControl-ReportSpam;Archived-At
  (define-advice notmuch-show-tag (:around (orig-fun &rest args) prompt-report-spam-around-notmuch-show-tag)
    "If tagging as spam then prompt to report to mailcontrol when supported"
    (let ((tag-changes (car args)))
      (when (seq-contains-p tag-changes "+spam" #'string=)
        (let ((subject (notmuch-show-get-subject))
              (sender (mail-extract-address-components (notmuch-show-get-from))))
          (when-let ((url (notmuch-show-get-header :X-MailControl-ReportSpam)))
            (and (apm-prompt-to-report-spam subject url)
                 (apm-prompt-to-add-email-to-forcepoint-blocklist (cadr sender) (car sender)))))))
    (apply orig-fun args))

  ;; requires to have set the following in ~/.notmuch-config so that the Archived-At header is available
  ;;
  ;; [show]
  ;; extra_headers=X-MailControl-ReportSpam;Archived-At
  (define-advice notmuch-show-stash-mlarchive-link (:around (orig-fun &rest args) use-archived-at-header-around-notmuch-show-stash-mlarchive-link)
    "Offer use of the Archived-At header if present."
    (let ((archived-at (notmuch-show-get-header :Archived-At)))
      (if archived-at
          (let ((notmuch-show-stash-mlarchive-link-alist
                 (append `(("Archived-At" . ,(lambda (id)
                                               ;; strip any leading and trailing </>
                                               (string-trim archived-at "<" ">"))))
                         notmuch-show-stash-mlarchive-link-alist))
                (notmuch-show-stash-ml-archive-link-default "Archived-At"))
            (apply orig-fun args))
        (apply orig-fun args))))

  ;; place sent in Sent/ maildir with sent tag and remove unread or inbox tags
  (setq notmuch-fcc-dirs "Sent +sent -unread -inbox")
  ;; place drafts in Drafts/ maildir
  (setq notmuch-draft-folder "Drafts")
  (setq notmuch-archive-tags '("-inbox" "-unread"))
  (setq mail-user-agent 'notmuch-user-agent)
  ;; ensure kernel team daily bug report emails display without wrapping
  (add-hook 'notmuch-show-insert-text/plain-hook 'notmuch-wash-convert-inline-patch-to-part)

  (defun apm-notmuch-wash-lp-build-log (_msg _depth)
    "Wash LP build logs in the current message."
    (apm-notmuch-show-view-lp-build-log))

  ;; automatically display and download failed LP build logs
  (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-lp-build-log)

  (defun apm-notmuch-wash-gfm (_msg _depth)
    "Format entire message as GFM if supported."
    ;; get entire message, use a tempt buffer to format it as GFM and then
    ;; replace message with that
    (when (fboundp 'gfm-mode)
      (let ((message (buffer-substring (point-min) (point-max))))
        (with-temp-buffer
          (delay-mode-hooks
            (gfm-mode))
          (insert message)
          (font-lock-ensure)
          (setq message (buffer-string)))
        (delete-region (point-min) (point-max))
        (insert message))))

  ;; TODO - make this configurable basd on the message itself and only run when
  ;; it looks like a plain text email with markdown contents
  ;; (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-gfm)

  ;; add gnus-art emphasis highlighting too
  (with-eval-after-load 'gnus-art
    (defun apm-notmuch-wash-article-emphasize (_msg _depth)
      (dolist (elem gnus-emphasis-alist)
        (let ((regexp (car elem))
              (invisible (nth 1 elem))
              (visible (nth 2 elem))
              (face (nth 3 elem))
              (props (append '(article-type emphasis)
                             gnus-hidden-properties)))
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (when (and (match-beginning visible) (match-beginning invisible))
              (gnus-article-hide-text
               (match-beginning invisible) (match-end invisible) props)
              (gnus-article-unhide-text-type
               (match-beginning visible) (match-end visible) 'emphasis)
              (gnus-put-overlay-excluding-newlines
               (match-beginning visible) (match-end visible) 'face face)
              (gnus-add-wash-type 'emphasis)
              (goto-char (match-end invisible)))))))

    (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-article-emphasize)
    ;; ensure hyphenated words are highlighted correctly
    (modify-syntax-entry ?- "w" notmuch-show-mode-syntax-table))

  (with-eval-after-load 'epa
    (defun apm-notmuch-wash-pgp-armor (_msg _depth)
      (let ((epa-replace-original-text t))
        (epa-decrypt-armor-in-region (point-min) (point-max))))
    (add-hook 'notmuch-show-insert-text/plain-hook 'apm-notmuch-wash-pgp-armor))

  ;; ensure when viewing parts we use a tmp dir which all snaps and regular
  ;; applications can access
  (setq mm-tmp-directory (expand-file-name "~/tmp"))
  (unless (file-exists-p mm-tmp-directory)
    (make-directory mm-tmp-directory))

  ;; periodically refresh all notmuch buffers every 5 minutes - actually
  ;; this causes point to move and so loses our place in the inbox buffer
  ;; when refresh happens so don't do this for now...
  (when nil
    (defvar apm-notmuch-refresh-timer nil)
    (when (timerp apm-notmuch-refresh-timer)
      (cancel-timer apm-notmuch-refresh-timer))
    (setq apm-notmuch-refresh-timer
          (run-at-time t 300 #'notmuch-refresh-all-buffers))

    ;; also ensure cursor doesn't move when notmuch buffers get refreshed
    (define-advice notmuch-refresh-this-buffer (:around (orig-fun &rest args) save-excursion-around-notmuch-refresh)
      "Save cursor position around notmuch-refresh-this-buffer."
      (save-excursion
        (apply orig-fun args))))

  ;; add a few helpful custom saved search queries
  (add-to-list 'notmuch-saved-searches '(:name "cvewebbot" :query "from:noreply+security-tools@canonical.com and subject:\"CVE webbot process errors\"" :key "c"))
  (add-to-list 'notmuch-saved-searches '(:name "emacs-devel" :query "tag:lists/emacs-devel and tag:inbox" :key "e"))
  (add-to-list 'notmuch-saved-searches '(:name "vince-updates" :query "from:cert+donotreply@cert.org and subject:\"New Post in Case Discussion\"" :key "v"))
  (dolist (rel '("noble" "oracular" "plucky"))
    (add-to-list 'notmuch-saved-searches `(:name ,(concat rel "-changes") :query ,(concat "tag:lists/" rel "-changes and tag:unread") :key ,(substring rel 0 1)))))

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

(use-package orderless
  ;; for vertico
  :ensure t)

(use-package org
  ;; use built-in version of org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c j" . consult-org-agenda)
         ("C-c C-w" . org-refile)
         :map org-mode-map
         ("M-s i" . consult-org-heading))
  ;; ensure we always load org at startup
  :demand t
  :preface
  (defun apm-org-mode-setup ()
    ;; add * = ~ as electric pairs
    (setq-local electric-pair-pairs (append electric-pair-pairs '((?\* . ?\*)
                                                                  (?\= . ?\=)
                                                                  (?\~ . ?\~)))))
  :hook
  (org-mode . apm-org-mode-setup)
  (org-mode . turn-on-auto-fill)
  :config
  (setq org-log-repeat nil)
  (setq org-log-into-drawer t)
  (setq org-pretty-entities t)
  ;; org-appear is disabled to show emphasis markers instead
  (setq org-hide-emphasis-markers nil)
  (setq org-directory (expand-file-name "~/git/org-files/"))
  (setq org-agenda-files (mapcar #'(lambda (f)
                                     (expand-file-name f org-directory))
                                 '("personal.org"
                                   "inbox.org" "tickler.org" "notes.org")))
  ;; don't indent org document sections etc
  (setq org-adapt-indentation nil)
  (setq org-imenu-depth 4)
  ;; @ = add note with time
  ;; ! = record only time of state change
  ;; | = remaining keywords are final states
  (setq org-todo-keywords '((sequence "TODO(t)" "WORK(w)" "REVW(r)" "|" "CANCELLED(c@)" "DELEGATED(G@)" "DONE")))
  ;; ensure it is harder to inadvertently delete collapsed parts of org
  ;; documents
  (setq org-catch-invisible-edits 'smart)
  (setq org-ctrl-k-protect-subtree t))

(use-package org-id
  :ensure org
  :config
  (setq org-id-link-to-org-use-id t))

(use-package org-autolist
  :ensure t
  :after org
  :hook (org-mode . org-autolist-mode))

(use-package org-crypt
  :ensure org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key '("88E9530BCBDDC200517B5EB0F498D2D9DE7DAD9C")))

(use-package org-refile
  :ensure org
  :config
  (setq org-refile-targets '(("~/git/org-files/personal.org" :maxlevel . 2)
                             ("~/git/org-files/someday.org" :level . 1)
                             ("~/git/org-files/tickler.org" :maxlevel . 1)
                             ("~/git/org-files/notes.org" :maxlevel . 2)))
  ;; allow to refile as top-level items in files
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil))

;; add support for man: links in org documents
(use-package ol-man
  :ensure org)

(use-package org-agenda
  :ensure org
  :custom
  ;; add remove-match so grid lines which are already present in an entry
  ;; are not shown
  (org-agenda-time-grid '((daily today require-timed remove-match)
                          (800 1000 1200 1400 1600 1800 2000)
                          "......" "----------------"))
  :config
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :narrow 80 :tags t :hidefiles t))
  ;; when showing agenda, jump to now
  (add-hook 'org-agenda-finalize-hook
            #'org-agenda-find-same-or-today-or-agenda 90)
  (setq org-agenda-custom-commands
        '(("i" "TODO from inbox" todo "TODO"
           ((org-agenda-files '("~/git/org-files/inbox.org"))))
          ("p" "TODO from personal" todo "TODO"
           ((org-agenda-files '("~/git/org-files/personal.org")))))))

(use-package org-capture
  :preface
  :after org
  :config
  (let ((inbox-org (expand-file-name "inbox.org" org-directory))
        (tickler-org (expand-file-name "tickler.org" org-directory)))
    (setq org-capture-templates
          `(("t" "todo" entry (file ,inbox-org)
             "* TODO %i%?
- %a")
            ("I" "tickler" entry (file ,tickler-org)
             "* %i%?
%U
- %a")
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
  :config
  ;; include the current clocked in task in clock reports
  (setq org-clock-report-include-clocking-task t)
  ;; save running clock and all history when exiting emacs
  (setq  org-clock-persist t)
  ;; resume clocking task on clock-in if the clock is open
  (setq  org-clock-in-resume t)
  ;; persist clock data into org-directory
  (setq org-clock-persist-file (expand-file-name "org-clock-save.el" org-directory))
  ;; reload any saved org clock information on startup
  (org-clock-persistence-insinuate))

(use-package org-clock-convenience
  :ensure t
  :after org
  :bind (:map org-agenda-mode-map
              ("S-<up>" . org-clock-convenience-timestamp-up)
              ("S-<down>" . org-clock-convenience-timestamp-down)))

(use-package org-duration
  :ensure org
  :after org
  ;; don't show days, only total hours as maximum value
  :config (setq org-duration-format (quote h:mm)))

(use-package orgit
  :ensure t
  :after org)

(use-package orgit-forge
  :ensure t)

(use-package org-src
  :ensure org
  :config
  ;; prefer org src blocks to not be indented with surrounding content
  (setq org-edit-src-content-indentation 0))

(use-package org-table-sticky-header
  :ensure t
  :after org
  :defer t
  :hook ((org-mode . org-table-sticky-header-mode)))

(use-package ox-gfm
  :ensure t
  :after ox)

(use-package paredit
  :ensure t
  ;; don't steal occur prefix
  :bind (:map paredit-mode-map ("M-s" . nil))
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

(use-package paren
  :defer t
  :commands show-paren-mode
  :custom
  (show-paren-context-when-offscreen 'child-frame)
  :config
  (show-paren-mode 1))

(use-package paren-face
  :ensure t
  :config (global-paren-face-mode 1))

(use-package pcap-mode
  :ensure t
  :mode ("\\.pcapng\\'" . pcap-mode))

(use-package pdf-tools
  :pin nongnu
  :ensure t
  :config
  (pdf-loader-install))

(use-package perl-mode
  :preface (defun apm-perl-mode-setup ()
             (setq-local indent-tabs-mode t))
  :hook ((perl-mode . apm-perl-mode-setup))
  :custom (perl-indent-level 8))

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

(use-package project
  :ensure t
  :pin gnu
  :demand t
  :bind ("<f5>" . project-compile)
  ;; try forcing magit to be integrated with project-switch-commands
  :config (with-eval-after-load 'magit
            (require 'magit-extras)))

(use-package project-mode-line-tag
  :ensure t
  :config (project-mode-line-tag-mode 1))

(use-package projection
  :ensure t
  :hook (after-init . global-projection-hook-mode)
  :hook (compilation-mode . projection-customize-compilation-mode)
  :custom
  (compilation-buffer-name-function 'projection-customize-compilation-buffer-name-function)
  :bind-keymap ("C-x P" . projection-map)
  :config (with-eval-after-load 'project
            (require 'projection)))

(use-package projection-multi
  :ensure t
  :after projection
  :bind (:map project-prefix-map
              ("RET" . projection-multi-compile)))

(use-package pydoc
  :ensure t
  :custom
  (pydoc-command "python3 -m pydoc")
  (pydoc-python-command "python3"))

(use-package python
  :defer t
  :init (unless (executable-find "ruff")
          (alert "Please snap install ruff"))
  :preface (defun apm-python-mode-setup ()
             "Remove python-flymake as we use flymake-ruff instead."
             (remove-hook 'flymake-diagnostic-functions 'python-flymake t))
  :hook ((python-mode . apm-python-mode-setup))
  :custom
  (python-indent-offset 4)
  (python-check-command (executable-find "ruff check")))

(use-package python-pytest
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook ((css-mode html-mode) . rainbow-mode))

(use-package recentf
  :preface
  (defun apm-recentf-ignore-sudo (file)
    "Do not keep FILE if it requires sudo to read."
    ;; return non-nil to keep in recentf-list
    (not (string-match "^/sudo:.*" file)))
  :custom (recentf-keep '(apm-recentf-ignore-sudo recentf-keep-default-predicate))
  :config (recentf-mode 1))

(use-package region-state
  :ensure t
  :config (region-state-mode 1))

(use-package rfn-eshadow
  :config
  ;; make minibuffer show shadowed files and play nicer with vertico
  (when (require 'vertico nil t)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))
  (file-name-shadow-mode 1))

(use-package ripgrep
  :ensure t
  :init (unless (executable-find "rg")
          (alert "Please apt install ripgrep")))

(use-package rnc-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  ;; ensure rust-analyzer is installed via the rustup snap
  :config
  (unless (executable-find "rust-analyzer")
    (alert "Please install the rustup snap")))

;; save minibuffer history
(use-package savehist
  :init (savehist-mode 1)
  :config (setq history-length 25))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (save-place-mode 1))

(use-package scratch
  :ensure t
  :defer t)

(use-package server
  :config
  ;; start emacs server only it has not already been started
  (unless (server-running-p)
    (server-start))
  (add-hook 'after-make-frame-functions
            #'(lambda (frame) (select-frame-set-input-focus frame)) t))

(use-package sh-script
  :init (setq-default sh-basic-offset 2
                      sh-indentation 2))

(use-package sideline
  :ensure t)

(use-package sideline-flymake
  :ensure t
  :hook (flymake-mode . sideline-mode)
  :custom
  (sideline-backends-right '((sideline-flymake . down))))

(use-package simple
  :defer t
  :init
  ;; save whatever is in the system clipboard to the kill ring before
  ;; killing something else into the kill ring
  (setq save-interprogram-paste-before-kill t)
  (setq next-error-message-highlight t)
  (setq visual-line-fringe-indicators
        '(left-curly-arrow right-curly-arrow)))

(use-package smiley
  :custom (smiley-style 'emoji))

(use-package so-long
  :config (global-so-long-mode 1))

(use-package strace-mode
  :ensure t)

(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))

(use-package suggest
  :ensure t)

(use-package syslog-mode
  :ensure t)

(use-package time
  :config
  (setq display-time-default-load-average nil)
  (setq display-time-use-mail-icon t)
  (setq display-time-day-and-date t)
  (setq display-time-24hr-format t)
  (display-time))

(use-package treesit-auto
  :ensure t
  :custom (treesit-auto-install 'prompt)
  :config (global-treesit-auto-mode 1))

(use-package udev-mode
  :ensure t)

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll/")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-after-kill-buffer-p t
                uniquify-ignore-buffers-re "^\\*"))

(use-package vc-auto-commit
  ;; used to automatically commit changes in org-files git repo
  :ensure t
  :config (vc-auto-commit-activate 1))

(use-package vertico
  :ensure t
  :hook ((emacs-startup . vertico-mode))
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package vertico-multiform
  :ensure vertico
  :config
  ;; enhance display of jinx to use a grid
  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (vertico-multiform-mode 1))

(use-package vertico-repeat
  :ensure vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("C-c r v" . vertico-repeat)))

(use-package vimrc-mode
  :ensure t
  :mode (("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package virtual-auto-fill
  :ensure t
  :hook (((markdown-mode rst-mode) . virtual-auto-fill-mode)))

(use-package vundo
  :ensure t)

(use-package webpaste
  :ensure t
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-region-or-region))
  :demand t
  :config
  (setq webpaste-paste-confirmation t)
  (setq webpaste-provider-priority '("dpaste.org")))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package whitespace
  :init (setq-default whitespace-style
                      '(face tabs tab-mark trailing missing-newline-at-eof))
  :config
  ;; whitespace-mode is not useful for magit-log buffers
  (setq whitespace-global-modes '(not magit-log-mode))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :config (global-whitespace-cleanup-mode 1))

(use-package window
  :preface
  (defun apm-select-split-window-right ()
    (interactive)
    (select-window (split-window-right)))
  (defun apm-select-split-window-below ()
    (interactive)
    (select-window (split-window-below)))
  :bind
  ;; use keybindings similar to ghostty for convenience
  ("C-S-o" . apm-select-split-window-right)
  ("C-S-e" . apm-select-split-window-below)
  ("s-ESC" . other-window))

(use-package wgrep-deadgrep
  :ensure t)

(use-package world-time-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package yaml-pro
  :ensure t)

(use-package yasnippet
  :ensure t
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
  :ensure t
  :custom (xref-search-program 'ripgrep))

(use-package ztree
  :ensure t
  :bind (("C-x C-d" . ztree-diff))
  :config
  (setq ztree-draw-unicode-lines t)
  (setq ztree-show-number-of-children t))

(provide 'init)

;;; init.el ends here
